module Fundraising.Calculations where

import Contract.Prelude

import Data.BigInt (BigInt, fromInt)
import Ext.Data.Ratio (mkBigIntRational, roundBigIntRatio)
import Shared.MinAda (minAda)

calcFee :: BigInt -> BigInt -> Maybe BigInt
calcFee feePercent funds' = do
  fee <- mkBigIntRational (feePercent * funds' /\ fromInt 100) -- Impossible to get a "division by zero" error
  rounded <- roundBigIntRatio fee
  let feeAmount = max rounded minAda
  pure feeAmount
