module StakingPool.StakingPoolScript where

import Contract.Monad (Contract, liftContractM)
import Contract.Prelude
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Value as Value
import Ext.Contract.Script (validatorScriptWithArg)
import Ext.Contract.Value (mkTokenName)
import StakingPool.Models (StakingPool)

foreign import stakingPoolValidator :: String

stakingPoolValidatorScript :: StakingPool -> Contract Validator
stakingPoolValidatorScript = validatorScriptWithArg stakingPoolValidator

getStakingPoolValidatorHash :: StakingPool -> Contract ValidatorHash
getStakingPoolValidatorHash stakingPool = do
  validator <- stakingPoolValidatorScript stakingPool
  pure $ validatorHash validator

getStakingPoolTokenName :: forall (r :: Row Type). Contract Value.TokenName
getStakingPoolTokenName = liftContractM "Cannot make StakingPool token name" $ stakingPoolTokenName

stakingPoolTokenNameString :: String
stakingPoolTokenNameString = "DonatPoolStakingPool"

stakingPoolTokenName :: Maybe Value.TokenName
stakingPoolTokenName = mkTokenName stakingPoolTokenNameString
