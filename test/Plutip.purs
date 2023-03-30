module Test.Plutip
  ( main
  ) where

import Prelude

import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (testPlutipContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.Aff (Milliseconds(Milliseconds), cancelWith, effectCanceler, launchAff)
import Mote (group)
import Test.Plutip.Common (config)
import Test.Plutip.Contracts.StartProtocol as StartProtocol
import Test.Spec.Runner (defaultConfig)

-- Run tests with 'spago run --main Test.Plutip'
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
      $ group "Plutip" do
          testPlutipContracts config $ do
            StartProtocol.suite
