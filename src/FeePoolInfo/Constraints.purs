module FeePoolInfo.Constraints where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.AssocMap as Map
import Contract.Credential (Credential(ScriptCredential))
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, MintingPolicyHash, Validator, ValidatorHash, mintingPolicyHash)
import Contract.Time (POSIXTime)
import Contract.Transaction (ScriptRef(..), mkTxUnspentOut)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Map as DataMap
import Ext.Contract.Time (TimeStamp, posixToTimeStamp)
import Ext.Contract.Value (mkCurrencySymbol)
import FeePool.Constraints as FeePool
import FeePool.Models (mkFeePoolFromProtocol)
import FeePoolInfo.Datum (PFeePoolInfoDatum(..))
import FeePoolInfo.FeePoolInfoScript (getFeePoolInfoValidatorHash, feePoolInfoValidatorScript)
import FeePoolInfo.Redeemer (PFeePoolInfoRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import MintingPolicy.VerTokenRedeemers (PVerTokenRedeemer(..))
import Protocol.Models (Protocol)
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Shared.MinAda (minAdaValue, twoMinAda)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO, filterByToken, getUtxoByScriptRef)

mkReceiveFundsConstraints :: Protocol -> POSIXTime -> BigInt -> Contract (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mkReceiveFundsConstraints protocol now feeAmt
  | feeAmt <= twoMinAda = pure $ mempty /\ mempty
  | otherwise = do
      let timestamp = posixToTimeStamp now
      currentEpoch <- FeePool.getFeePoolEpoch protocol
      feePool <- mkFeePoolFromProtocol protocol
      feePoolInfoHash <- getFeePoolInfoValidatorHash feePool
      feePoolInfoValidator <- feePoolInfoValidatorScript feePool
      verPolicy /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)

      if timestamp.epoch == currentEpoch then mkCurrentConstraints timestamp feeAmt feePoolInfoHash feePoolInfoValidator verTokenCs
      else mkNewConstraints protocol timestamp feeAmt feePoolInfoHash verPolicy verTokenCs

mkCurrentConstraints
  :: TimeStamp
  -> BigInt
  -> ValidatorHash
  -> Validator
  -> Value.CurrencySymbol
  -> Contract (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mkCurrentConstraints timestamp feeAmt feePoolInfoHash feePoolInfoValidator verTokenCs = do
  networkId <- getNetworkId
  feePoolAddress <- liftContractM "Impossible to get FeePoolInfo script address" $ validatorHashBaseAddress networkId feePoolInfoHash
  feePoolUTxOs <- utxosAt feePoolAddress

  let feePoolScriptRef = PlutusScriptRef $ unwrap feePoolInfoValidator
  feePoolRefScriptUtxo <- getUtxoByScriptRef "FeePoolInfo" feePoolScriptRef feePoolUTxOs
  feePoolVerTokenName <- VerToken.feePoolVerTokenName

  let filteredUTxOs = filterByToken (verTokenCs /\ feePoolVerTokenName) $ DataMap.toUnfoldable feePoolUTxOs
  utxo <- liftContractM "FeePoolInfo UTxO for the current epoch not found" $ getOneUTxOByEpoch timestamp.epoch filteredUTxOs
  PFeePoolInfoDatum datum <- liftContractM "Impossible to extract datum from FeePoolInfo UTxO" $ extractDatumFromUTxO utxo
  let
    updatedFee = case Map.lookup timestamp.dayOfEpoch datum.fee of
      Just amt -> Map.insert timestamp.dayOfEpoch (amt + feeAmt) datum.fee
      _ -> Map.insert timestamp.dayOfEpoch feeAmt datum.fee

    updatedDatum = Datum <<< toData $ PFeePoolInfoDatum (datum { fee = updatedFee })

    feePoolefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst feePoolRefScriptUtxo) (snd feePoolRefScriptUtxo)

    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst utxo)
        (Redeemer $ toData PAddRecord)
        feePoolefScriptInput
        <> Constraints.mustPayToScriptAddress
          feePoolInfoHash
          (ScriptCredential feePoolInfoHash)
          updatedDatum
          Constraints.DatumInline
          (extractValueFromUTxO utxo)
        <> Constraints.mustReferenceOutput (fst feePoolRefScriptUtxo)
    lookups = Lookups.unspentOutputs feePoolUTxOs
  pure $ constraints /\ lookups

mkNewConstraints
  :: Protocol
  -> TimeStamp
  -> BigInt
  -> ValidatorHash
  -> MintingPolicy
  -> Value.CurrencySymbol
  -> Contract (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mkNewConstraints protocol timestamp feeAmt feePoolInfoHash verPolicy verTokenCs = do
  feePoolVerTokenName <- VerToken.feePoolVerTokenName
  let
    verTokenPolicyHash :: MintingPolicyHash
    verTokenPolicyHash = mintingPolicyHash verPolicy
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol

  let
    records = Map.singleton timestamp.dayOfEpoch feeAmt
    datum = PFeePoolInfoDatum
      { epoch: timestamp.epoch
      , fee: records
      }
    payment = Value.singleton verTokenCs feePoolVerTokenName one <> minAdaValue
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        verTokenPolicyHash
        (Redeemer $ toData PMintFeePoolVerToken)
        feePoolVerTokenName
        one
        protocolInfo.references.verTokenInput
        <> Constraints.mustPayToScriptAddress
          feePoolInfoHash
          (ScriptCredential feePoolInfoHash)
          (Datum $ toData datum)
          Constraints.DatumInline
          payment
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.verTokenRef)

    lookups = Lookups.unspentOutputs protocolInfo.pUtxos

  pure $ constraints /\ lookups

getOneUTxOByEpoch :: BigInt -> Array UtxoTuple -> Maybe UtxoTuple
getOneUTxOByEpoch currentEpoch utxoArray = do
  let filtered = Array.filter (isExpectedEpoch currentEpoch) utxoArray
  { head, tail } <- Array.uncons filtered
  if Array.null tail then Just head else Nothing

isExpectedEpoch :: BigInt -> UtxoTuple -> Boolean
isExpectedEpoch currentEpoch utxo =
  case extractDatumFromUTxO utxo of
    Just (PFeePoolInfoDatum datum) -> datum.epoch == currentEpoch
    _ -> false
