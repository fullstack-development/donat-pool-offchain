module Test.Unit.CalcFee (suite) where

import Prelude

import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.BigInt (fromInt)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Fundraising.Calculations as Calculations
import Mote (test)
import Test.Utils (assertTrue)

suite :: TestPlanM (Aff Unit) Unit
suite = test "CalcFee tests" do
  assertTrue "CalcFee 30% from 10_000_000 is Just 3_000_000"
   (Calculations.calcFee (fromInt 30) (fromInt 10_000_000) == Just (fromInt 3_000_000))
  assertTrue "CalcFee 10% from 10_000_000 is Just 2_000_000 as 1_000_000 is less than minAda"
   (Calculations.calcFee (fromInt 10) (fromInt 10_000_000) == Just (fromInt 2_000_000))
  assertTrue "CalcFee 0% from 10_000_000 is Just 2_000_000 as 0 is less than minAda"
   (Calculations.calcFee (fromInt 0) (fromInt 10_000_000) == Just (fromInt 2_000_000))
