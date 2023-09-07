module FeePoolInfo.FeePoolInfoScript where

import Contract.Monad (Contract)
import Contract.Prelude
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Ext.Contract.Script (validatorScriptWithArg)
import FeePool.Models (FeePool)

foreign import feePoolInfoValidator :: String

feePoolInfoValidatorScript :: FeePool -> Contract Validator
feePoolInfoValidatorScript = validatorScriptWithArg feePoolInfoValidator

getFeePoolInfoValidatorHash :: FeePool -> Contract ValidatorHash
getFeePoolInfoValidatorHash feePool = do
  validator <- feePoolInfoValidatorScript feePool
  pure $ validatorHash validator
