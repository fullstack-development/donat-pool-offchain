module CLI.CheckEpoch.Main where

import Prelude

import Effect (Effect)
import StakingPool.CheckEpoch (runCheckEpochFromCli)

main :: Effect Unit
main = runCheckEpochFromCli
