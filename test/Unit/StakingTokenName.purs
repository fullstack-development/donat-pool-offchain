module Test.Unit.StakingTokenName (suite) where

import Prelude

import Contract.Value as Value
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.BigInt (fromInt)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Ext.Contract.Value (mkTokenName)
import Mote (test)
import StakingPool.TokenName as StakingPool
import Test.Utils (assertTrue)

justTokenName :: Maybe Value.TokenName
justTokenName = mkTokenName "DPStake11.42.99"

parseTokenNameCase :: Maybe StakingPool.Receipt
parseTokenNameCase = do
  tn <- justTokenName
  StakingPool.parseReceiptTokenName tn

receipt :: StakingPool.Receipt
receipt = { time: { epoch: fromInt 11, dayOfEpoch: fromInt 42 }, amount: (fromInt 99) }

doubleConvertingCase :: Maybe StakingPool.Receipt
doubleConvertingCase = do
  tn <- StakingPool.receiptTokenName receipt
  StakingPool.parseReceiptTokenName tn

suite :: TestPlanM (Aff Unit) Unit
suite = test "StakingTokenName tests" do
  assertTrue "Should successfully create TokenName from provided data"
    (StakingPool.receiptTokenName receipt == justTokenName && isJust justTokenName)
  assertTrue "Should successfully parse Recept data from provided TokenName"
    (parseTokenNameCase == Just receipt)
  assertTrue "Should return same receipt after making tokenName"
    (doubleConvertingCase == Just receipt)
