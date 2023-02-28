module Shared.MinAda where

import Contract.Prelude
import Contract.Value (Value, lovelaceValueOf)
import Data.BigInt (fromInt)

minAdaValue :: Value
minAdaValue = lovelaceValueOf $ fromInt 2_000_000