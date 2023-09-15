module CLI.OpenNewEpoch.Main where

import Prelude

import Effect (Effect)
import StakingPool.OpenNewEpoch (runOpenNewEpochFromCli)

main :: Effect Unit
main = runOpenNewEpochFromCli
