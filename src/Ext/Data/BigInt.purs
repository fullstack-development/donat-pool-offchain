module Ext.Data.BigInt where

import Prelude
import Data.BigInt (BigInt, toString)

-- Eq for BigInt and Number doesn't work :(
eqBigInt :: BigInt -> BigInt -> Boolean
eqBigInt n1 n2 = toString n1 == toString n2
