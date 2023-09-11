module Ext.Data.Boolean where

import Prelude

import Data.BigInt (BigInt, fromInt)

bigIntToBoolean :: BigInt -> Boolean
bigIntToBoolean b = 
    if b == fromInt 0 
        then false
        else true

booleanToBigInt :: Boolean -> BigInt
booleanToBigInt true = fromInt 1
booleanToBigInt false = fromInt 0
