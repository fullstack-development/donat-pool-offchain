module Fundraising.Calculations where

import Contract.Prelude

import Data.BigInt (BigInt, fromInt)
import Shared.Helpers (mkBigIntRational, roundBigIntRatio)
import Shared.MinAda (minAda)

calcFee :: BigInt -> BigInt -> Maybe BigInt
calcFee feePercent funds' = do
  fee <- mkBigIntRational (feePercent * funds' /\ fromInt 100) -- Impossible to get an error as `fromInt 100 /= 0`
  rounded <- roundBigIntRatio fee
  let feeAmount = max rounded minAda
  pure feeAmount
