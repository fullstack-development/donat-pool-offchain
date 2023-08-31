module FeePool.FeePoolInfoScript where

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prelude
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs, validatorHash, ValidatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (singleton) as Array
import Effect.Exception (error)
import FeePool.Models (FeePool)

foreign import feePoolInfoValidator :: String

feePoolInfoValidatorScript :: FeePool -> Contract Validator
feePoolInfoValidatorScript feePool = do
  script <- liftMaybe (error "Error decoding feePoolInfoValidator") do
    envelope <- decodeTextEnvelope feePoolInfoValidator
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkFeePooInfolValidatorScript script feePool
  pure $ Validator res

mkFeePooInfolValidatorScript :: PlutusScript -> FeePool -> Either ApplyArgsError PlutusScript
mkFeePooInfolValidatorScript unappliedValidator feePool =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData feePool)
  in
    applyArgs unappliedValidator validatorArgs

getFeePoolInfoValidatorHash :: FeePool -> Contract ValidatorHash
getFeePoolInfoValidatorHash feePool = do
  validator <- feePoolInfoValidatorScript feePool
  pure $ validatorHash validator
