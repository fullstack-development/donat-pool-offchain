module Fundraising.Donate where

import Contract.Prelude

import Contract.Address (AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Config (NetworkId(TestnetId))
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.Datum (Datum(..))
import Ctl.Internal.Types.Interval (mkFiniteInterval)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.Models (Fundraising(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import Shared.Helpers (checkTokenInUTxO, getNonCollateralUtxo)
import Shared.MinAda (minAdaValue)
import Shared.RunContract (runContractWithUnitResult)
import Fundraising.FundrisingScriptInfo (FundrisingScriptInfo(..), getFundrisingScriptInfo, makeFundrising)

runDonate :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> FundraisingData -> BigInt -> Effect Unit
runDonate onComplete onError fundraisingData amount =
  runContractWithUnitResult onComplete onError $ contract fundraisingData amount

contract :: FundraisingData -> BigInt -> Contract () Unit
contract frData@(FundraisingData fundraisingData) amount = do
  logInfo' "Running donate"

  let
    threadTokenCurrency = fundraisingData.frThreadTokenCurrency
    threadTokenName = fundraisingData.frThreadTokenName
  fundraising@(Fundraising fr) <- makeFundrising frData
  (FundrisingScriptInfo frInfo) <- getFundrisingScriptInfo fundraising threadTokenCurrency threadTokenName
  let isVerTokenInUtxo = checkTokenInUTxO (fr.verTokenCurrency /\ fr.verTokenName) frInfo.frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  let
    currentFunds = frInfo.frValue
    (PFundraisingDatum currentDatum) = frInfo.frDatum

  now <- currentTime
  let deadline = currentDatum.frDeadline
  let amountToRaise = currentDatum.frAmount
  let currentDonationsAmount = Value.valueToCoin' currentFunds - Value.valueToCoin' minAdaValue - Value.valueToCoin' minAdaValue
  when (now > deadline) $ throw >>> liftEffect $ "fundraising time is over"
  when (currentDonationsAmount >= amountToRaise) $ throw >>> liftEffect $ "fundraising goal is already completed"

  donatorHashes <- ownPaymentPubKeysHashes
  donatorPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head donatorHashes
  donatorAddress <- liftedM "Failed to get donator address" $ Array.head <$> getWalletAddresses
  donatorUtxo <- utxosAt donatorAddress >>= getNonCollateralUtxo

  let newDatum = Datum $ toData frInfo.frDatum
  let donation = Value.singleton Value.adaSymbol Value.adaToken amount
  let newValue = currentFunds <> donation
  let donateRedeemer = Redeemer $ toData $ PDonate threadTokenCurrency threadTokenName amount
  let donationTimeRange = mkFiniteInterval now deadline

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst frInfo.frUtxo) donateRedeemer
        <> Constraints.mustPayToScriptAddress frInfo.frValidatorHash (ScriptCredential frInfo.frValidatorHash) newDatum Constraints.DatumInline newValue
        <> Constraints.mustBeSignedBy donatorPkh
        <> Constraints.mustValidateIn donationTimeRange

  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.validator frInfo.frValidator
        <> Lookups.unspentOutputs frInfo.frUtxos
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
