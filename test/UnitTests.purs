module Test.UnitTests (main, testPlan) where

import Prelude

import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff (Aff, cancelWith, effectCanceler, launchAff)
import Test.Spec.Runner (defaultConfig)
import Test.Unit.CalcFee as CalcFee
import Test.Unit.Serialization as Serialization

-- Run with `spago test --main Test.UnitTests`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
      testPlan

testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  CalcFee.suite
  Serialization.suite
