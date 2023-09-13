module StakingPoolInfo.StakingPoolInfoScript where

import Contract.Monad (Contract)
import Contract.Prelude
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Ext.Contract.Script (validatorScriptWithArg)
import StakingPool.Models (StakingPool)

foreign import stakingPoolInfoValidator :: String

stakingPoolInfoValidatorScript :: StakingPool -> Contract Validator
stakingPoolInfoValidatorScript = validatorScriptWithArg stakingPoolInfoValidator

getStakingPoolInfoValidatorHash :: StakingPool -> Contract ValidatorHash
getStakingPoolInfoValidatorHash stakingPool = do
  validator <- stakingPoolInfoValidatorScript stakingPool
  pure $ validatorHash validator
