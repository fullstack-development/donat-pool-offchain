module FeePool.Constraints where

import Contract.Prelude

import Contract.Credential (Credential(ScriptCredential))
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (BigInt)
import Ext.Contract.Time (posixToTimeStamp)
import FeePool.Datum (PFeePoolDatum(..))
import FeePool.Redeemer (PFeePoolRedeemer(..))
import Protocol.Models (Protocol)
import Shared.MinAda (minAda, twoMinAda)
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
