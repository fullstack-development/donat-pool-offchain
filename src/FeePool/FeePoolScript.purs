module FeePool.FeePoolScript where

import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Value as Value
import Ext.Contract.Script (validatorScriptWithArg)
import Ext.Contract.Value (mkTokenName)
import FeePool.Models (FeePool)

foreign import feePoolValidator :: String

feePoolValidatorScript :: FeePool -> Contract Validator
feePoolValidatorScript = validatorScriptWithArg feePoolValidator

getFeePoolValidatorHash :: FeePool -> Contract ValidatorHash
getFeePoolValidatorHash feePool = do
  validator <- feePoolValidatorScript feePool
  pure $ validatorHash validator

getFeePoolTokenName :: forall (r :: Row Type). Contract Value.TokenName
getFeePoolTokenName = liftContractM "Cannot make FeePool token name" $ feePoolTokenName

feePoolTokenNameString :: String
feePoolTokenNameString = "DonatPoolFeePool"

feePoolTokenName :: Maybe Value.TokenName
feePoolTokenName = mkTokenName feePoolTokenNameString
