module FeePool.Constraints where

import Contract.Prelude

import Contract.Credential (Credential(ScriptCredential))
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)
import Ext.Contract.Time (posixToTimeStamp)
import FeePool.Datum (PFeePoolDatum(..))
import FeePool.FeePoolScript (getFeePoolTokenName, getFeePoolValidatorHash)
import FeePool.Models (mkFeePoolFromProtocol)
import FeePool.Redeemer (PFeePoolRedeemer(..))
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Models (Protocol(..))
import Shared.MinAda (minAda, minAdaValue, twoMinAda)
import Shared.ScriptInfo (ScriptInfo(..), getFeePoolScriptInfo)

getFeePoolEpoch :: Protocol -> Contract BigInt
getFeePoolEpoch protocol = do
  (ScriptInfo feePoolScriptInfo :: ScriptInfo PFeePoolDatum) <- getFeePoolScriptInfo protocol
  pure $ (unwrap feePoolScriptInfo.datum).currentEpoch

mkReceiveFundsConstraints :: Protocol -> POSIXTime -> BigInt -> Contract (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mkReceiveFundsConstraints protocol now feeAmt
  | feeAmt <= twoMinAda = pure $ mempty /\ mempty
  | otherwise = do
      let timestamp = posixToTimeStamp now
      (ScriptInfo feePoolScriptInfo :: ScriptInfo PFeePoolDatum) <- getFeePoolScriptInfo protocol
      let
        isCurrentEpoch = (unwrap feePoolScriptInfo.datum).currentEpoch == timestamp.epoch
        amtToDeposit = if isCurrentEpoch then feeAmt - minAda else feeAmt - twoMinAda
        redeemer =
          if isCurrentEpoch then PAddFundsWithCurrentEpoch amtToDeposit
          else PAddFundsWithNewEpoch amtToDeposit
        newDatum = Datum <<< toData $ PFeePoolDatum { currentEpoch: timestamp.epoch }
        paymentToFeePool = feePoolScriptInfo.value <> Value.lovelaceValueOf amtToDeposit
      let
        constraints =
          Constraints.mustSpendScriptOutputUsingScriptRef
            (fst feePoolScriptInfo.utxo)
            (Redeemer $ toData redeemer)
            feePoolScriptInfo.refScriptInput
            <> Constraints.mustPayToScriptAddress
              feePoolScriptInfo.validatorHash
              (ScriptCredential feePoolScriptInfo.validatorHash)
              newDatum
              Constraints.DatumInline
              paymentToFeePool
            <> Constraints.mustReferenceOutput (fst feePoolScriptInfo.refScriptUtxo)
      let lookups = Lookups.unspentOutputs feePoolScriptInfo.utxos
      pure $ constraints /\ lookups

mkStartSystemConstraints :: Protocol -> Contract (Constraints.TxConstraints Void Void)
mkStartSystemConstraints protocol'@(Protocol protocol) = do
  feePoolTn <- getFeePoolTokenName
  feePool <- mkFeePoolFromProtocol protocol'
  feePoolHash <- getFeePoolValidatorHash feePool
  let
    initDatum = PFeePoolDatum { currentEpoch: fromInt 0 }
    feePoolTokenValue = Value.singleton protocol.protocolCurrency feePoolTn one
    payment = minAdaValue <> feePoolTokenValue

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer $ toData $ PMintNft feePoolTn)
        feePoolTokenValue
        <> Constraints.mustPayToScriptAddress
          feePoolHash
          (ScriptCredential feePoolHash)
          (Datum $ toData initDatum)
          Constraints.DatumInline
          payment

  pure constraints