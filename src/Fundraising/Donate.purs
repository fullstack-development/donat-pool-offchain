module Fundraising.Donate where

import Contract.Prelude

import Ctl.Internal.Types.Interval (Interval(..))
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
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Shared.Helpers (extractDatumFromUTxO, extractValueFromUTxO, getNonCollateralUtxo, getUtxoByNFT)

runDonate :: Fundraising -> BigInt -> Effect Unit
runDonate fundrising amount = launchAff_ do
  runContract testnetNamiConfig (contract fundrising amount)

contract :: Fundraising -> BigInt -> Contract () Unit
contract fundrising'@(Fundraising fundrising) amount = do
  logInfo' "Running donate"
  frValidator <- fundraisingValidatorScript fundrising'
  frValidatorHash <- getFundraisingValidatorHash fundrising'
  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress
  frUtxo <- getUtxoByNFT "Fundraising" (Tuple (_.verTokenCurrency fundrising) (_.verTokenName fundrising)) frUtxos

  currentDatum'@(PFundraisingDatum currentDatum) <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  let currentFunds = extractValueFromUTxO frUtxo
  logInfo' $ "Current funds: " <> show currentFunds

  now <- currentTime
  let deadline = _.frDeadline currentDatum
  when (now > deadline) $ liftEffect $ throw "Fundrising time is over"

  donatorHashes <- ownPaymentPubKeysHashes
  donatorPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head donatorHashes
  donatorAddress <- liftedM "Failed to get donator address" $ Array.head <$> getWalletAddresses
  donatorUtxo <- utxosAt donatorAddress >>= getNonCollateralUtxo

  let newDatum = Datum $ toData currentDatum'
  let donation = Value.singleton Value.adaSymbol Value.adaToken amount
  let newValue = currentFunds <> donation
  let donateRedeemer = Redeemer $ toData $ PDonate amount
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
