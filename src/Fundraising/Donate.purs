module Fundraising.Donate where

import Contract.Prelude

import Contract.Address (AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes, validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Config (testnetNamiConfig, NetworkId(TestnetId))
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.Datum (Datum(..))
import Ctl.Internal.Types.Interval (Interval(..))
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import MintingPolicy.VerTokenMinting as VerToken
import Shared.Helpers (extractDatumFromUTxO, extractValueFromUTxO, getNonCollateralUtxo, getUtxoByNFT, checkTokenInUTxO, mkCurrencySymbol)
import Shared.MinAda (minAdaValue)

runDonate :: FundraisingData -> BigInt -> Effect Unit
runDonate fundraisingData amount = launchAff_ do
  runContract testnetNamiConfig (contract fundraisingData amount)

contract :: FundraisingData -> BigInt -> Contract () Unit
contract (FundraisingData fundraisingData) amount = do
  logInfo' "Running donate"

  let
    givenProtocol = fundraisingData.protocol
    threadTokenCurrency = fundraisingData.frThreadTokenCurrency
    threadTokenName = fundraisingData.frThreadTokenName

  _ /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy givenProtocol)
  verTn <- VerToken.verTokenName

  let
    fundraising = Fundraising
      { protocol: givenProtocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }

  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising
  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress
  frUtxo <- getUtxoByNFT "Fundraising" (threadTokenCurrency /\ threadTokenName) frUtxos
  let isVerTokenInUtxo = checkTokenInUTxO (verTokenCs /\ verTn) frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  currentDatum'@(PFundraisingDatum currentDatum) <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  let currentFunds = extractValueFromUTxO frUtxo
  logInfo' $ "Current funds: " <> show currentFunds

  now <- currentTime
  let deadline = currentDatum.frDeadline
  let amountToRaise = currentDatum.frAmount
  let currentDonationsAmount = Value.valueToCoin' currentFunds - Value.valueToCoin' minAdaValue
  when (now > deadline) $ throw >>> liftEffect $ "fundraising time is over"
  when (currentDonationsAmount >= amountToRaise) $ throw >>> liftEffect $ "fundraising goal is already completed"
  when (currentDonationsAmount + amount > amountToRaise) $ throw >>> liftEffect $ "your donation exceeds the fundraising goal"

  donatorHashes <- ownPaymentPubKeysHashes
  donatorPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head donatorHashes
  donatorAddress <- liftedM "Failed to get donator address" $ Array.head <$> getWalletAddresses
  donatorUtxo <- utxosAt donatorAddress >>= getNonCollateralUtxo

  let newDatum = Datum $ toData currentDatum'
  let donation = Value.singleton Value.adaSymbol Value.adaToken amount
  let newValue = currentFunds <> donation
  let donateRedeemer = Redeemer $ toData $ PDonate threadTokenCurrency threadTokenName amount
  let donationTimeRange = FiniteInterval now deadline

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst frUtxo) donateRedeemer
        <> Constraints.mustPayToScriptAddress frValidatorHash (ScriptCredential frValidatorHash) newDatum Constraints.DatumInline newValue
        <> Constraints.mustBeSignedBy donatorPkh
        <> Constraints.mustValidateIn donationTimeRange

  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.validator frValidator
        <> Lookups.unspentOutputs frUtxos
        <> Lookups.unspentOutputs donatorUtxo

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    addressWithNetworkTag =
      AddressWithNetworkTag
        { address: donatorAddress
        , networkId: TestnetId
        }

    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' "Donate finished successfully"
