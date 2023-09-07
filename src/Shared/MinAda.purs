module Shared.MinAda where

import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)
import Contract.Prelude

minAdaInt :: Int
minAdaInt = 2_000_000

minAda :: BigInt
minAda = fromInt minAdaInt

twoMinAda :: BigInt
twoMinAda = minAda + minAda

minAdaValue :: Value.Value
minAdaValue = Value.lovelaceValueOf minAda

sevenMinAdaValue ∷ Value.Value
sevenMinAdaValue = Value.lovelaceValueOf (minAda * (fromInt 7))