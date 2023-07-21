module Fundraising.Donate where

import Contract.Prelude

import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Types.Datum (Datum(..))
import Ctl.Internal.Types.Interval (from)
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScriptInfo (FundraisingScriptInfo(..), getFundraisingScriptInfo, makeFundraising)
import Fundraising.Models (Fundraising(..))
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import Shared.Utxo (checkTokenInUTxO)
import Shared.MinAda (minAdaValue)
import Shared.NetworkData (NetworkParams)
import Shared.RunContract (runContractWithResult)
import Protocol.UserData (ProtocolData)

runDonate :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> FundraisingData -> Int -> Effect Unit
runDonate onComplete onError pData networkParams fundraisingData amount =
  runContractWithResult onComplete onError networkParams $ contract pData fundraisingData amount

contract :: ProtocolData -> FundraisingData -> Int -> Contract Unit
contract pData (FundraisingData fundraisingData) adaAmount = do
  logInfo' "Running donate"
  let
    threadTokenCurrency = fundraisingData.frThreadTokenCurrency
    threadTokenName = fundraisingData.frThreadTokenName
  fundraising@(Fundraising fr) <- makeFundraising pData
  (FundraisingScriptInfo frInfo) <- getFundraisingScriptInfo fundraising threadTokenCurrency threadTokenName
  let isVerTokenInUtxo = checkTokenInUTxO (fr.verTokenCurrency /\ fr.verTokenName) frInfo.frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  let
    currentFunds = frInfo.frValue
    (PFundraisingDatum currentDatum) = frInfo.frDatum

  now <- currentTime
  let amount = fromInt adaAmount * fromInt 1_000_000
  let deadline = currentDatum.frDeadline
  let amountToRaise = currentDatum.frAmount
  let currentDonationsAmount = Value.valueToCoin' currentFunds - Value.valueToCoin' minAdaValue - Value.valueToCoin' minAdaValue
  when (now > deadline) $ throw >>> liftEffect $ "fundraising time is over"
  when (currentDonationsAmount >= amountToRaise) $ throw >>> liftEffect $ "fundraising goal is already completed"

  (OwnCredentials creds) <- getOwnCreds
  let newDatum = Datum $ toData frInfo.frDatum
  let donation = Value.singleton Value.adaSymbol Value.adaToken amount
  let newValue = currentFunds <> donation
  let donateRedeemer = Redeemer $ toData $ PDonate threadTokenCurrency threadTokenName amount
  let donationTimeRange = from now

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst frInfo.frUtxo)
        donateRedeemer
        frInfo.frRefScriptInput
        <> Constraints.mustPayToScriptAddress
          frInfo.frValidatorHash
          (ScriptCredential frInfo.frValidatorHash)
          newDatum
          Constraints.DatumInline
          newValue
        <> Constraints.mustBeSignedBy creds.ownPkh
        <> Constraints.mustValidateIn donationTimeRange
        <> Constraints.mustReferenceOutput (fst frInfo.frScriptRef)

  let
    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs frInfo.frUtxos

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress creds.ownAddressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' "Donate finished successfully"
