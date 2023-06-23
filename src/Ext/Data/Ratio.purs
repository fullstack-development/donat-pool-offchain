module Ext.Data.Ratio where

import Contract.Prelude

import Data.BigInt (fromInt, BigInt)
import Data.BigInt as BigInt
import Data.Rational ((%), Ratio, numerator, denominator)
import Math as Math

mkRational :: Tuple Int Int -> Maybe (Ratio BigInt)
mkRational (Tuple num den) =
  if den == 0 then Nothing
  else Just (fromInt num % fromInt den)

mkBigIntRational :: Tuple BigInt BigInt -> Maybe (Ratio BigInt)
mkBigIntRational (Tuple num den) =
  if den == fromInt 0 then Nothing
  else Just (num % den)

roundBigIntRatio :: Ratio BigInt -> Maybe BigInt
roundBigIntRatio frac = Math.round >>> BigInt.fromNumber $ bigIntRatioToNumber frac

bigIntRatioToNumber :: Ratio BigInt -> Number
bigIntRatioToNumber x = BigInt.toNumber (numerator x) / BigInt.toNumber (denominator x)
