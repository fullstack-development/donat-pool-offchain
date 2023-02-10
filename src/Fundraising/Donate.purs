module Fundraising.Donate where

import Contract.Prelude

import Contract.Address (AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes, validatorHashBaseAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract, launchAff_, liftContractM, liftedE, liftedM, runContract)
import Data.BigInt (BigInt)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Shared.Helpers (extractDatumFromUTxO, extractValueFromUTxO, getNonCollateralUtxo, getUtxoByNFT)
import Contract.Log (logInfo')
import Contract.Utxos (utxosAt)
import Data.Array (head) as Array
import Effect.Exception (throw)
import Ctl.Internal.Types.Datum (Datum(..))
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.Value as Value
import Contract.TxConstraints as Constraints
import Contract.Credential (Credential(ScriptCredential))
import Contract.ScriptLookups as Lookups
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.Config (testnetNamiConfig, NetworkId(TestnetId))

runDonate :: Fundraising -> BigInt -> Effect Unit
runDonate fundrising amount = launchAff_ do
  runContract testnetNamiConfig (contract fundrising amount)

contract :: Fundraising -> BigInt -> Contract () Unit
contract fundrising'@(Fundraising fundrising) amount = do
  logInfo' "Running donate"
  frValidator <- fundraisingValidatorScript fundrising'
  frValidatorHash <- getFundraisingValidatorHash fundrising'
  frAddress <- liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress
  frUtxo <- getUtxoByNFT "Fundraising" (Tuple (_.verTokenCurrency fundrising) (_.verTokenName fundrising)) frUtxos

  currentDatum'@(PFundraisingDatum currentDatum) <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  let currentFunds = extractValueFromUTxO frUtxo
  logInfo' $ "Current funds: " <> show currentFunds

  now <- currentTime
  when (now > _.frDeadline currentDatum) $ liftEffect $ throw "Fundrising time is over"

  donatorHashes <- ownPaymentPubKeysHashes
  donatorPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head donatorHashes
  donatorAddress <- liftedM "Failed to get donator address" $ Array.head <$> getWalletAddresses
  donatorUtxo <- utxosAt donatorAddress >>= getNonCollateralUtxo

  let newDatum = Datum $ toData currentDatum'
  let donaition = Value.singleton Value.adaSymbol Value.adaToken amount
  let newValue = currentFunds <> donaition
  let donateRedeemer = Redeemer $ toData $ PDonate (_.verTokenCurrency fundrising) (_.verTokenName fundrising) amount

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst frUtxo) donateRedeemer
        <> Constraints.mustPayToScriptAddress frValidatorHash (ScriptCredential frValidatorHash) newDatum Constraints.DatumInline newValue
        <> Constraints.mustBeSignedBy donatorPkh

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
