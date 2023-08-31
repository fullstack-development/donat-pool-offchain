module FeePool.FeePoolScript where

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prelude
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs, validatorHash, ValidatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (singleton) as Array
import Effect.Exception (error)
import FeePool.Models (FeePool)
import Ext.Contract.Value (mkTokenName)

foreign import feePoolValidator :: String

feePoolValidatorScript :: FeePool -> Contract Validator
feePoolValidatorScript feePool = do
  script <- liftMaybe (error "Error decoding feePoolValidator") do
    envelope <- decodeTextEnvelope feePoolValidator
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkFeePoolValidatorScript script feePool
  pure $ Validator res

mkFeePoolValidatorScript :: PlutusScript -> FeePool -> Either ApplyArgsError PlutusScript
mkFeePoolValidatorScript unappliedValidator feePool =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData feePool)
  in
    applyArgs unappliedValidator validatorArgs

getFeePoolValidatorHash :: FeePool -> Contract ValidatorHash
getFeePoolValidatorHash feePool = do
  validator <- feePoolValidatorScript feePool
  pure $ validatorHash validator

getFeePoolTokenName :: forall (r :: Row Type). Contract Value.TokenName
getFeePoolTokenName = liftContractM "Cannot make Fundraising token name" $ feePoolTokenName

feePoolTokenNameString :: String
feePoolTokenNameString = "DonatPoolFeePool"

feePoolTokenName :: Maybe Value.TokenName
feePoolTokenName = mkTokenName feePoolTokenNameString
