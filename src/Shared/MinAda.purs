module Shared.MinAda where

import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)

minAdaInt :: Int
minAdaInt = 2_000_000

minAda :: BigInt
minAda = fromInt minAdaInt

minAdaValue :: Value.Value
minAdaValue = Value.lovelaceValueOf minAda

