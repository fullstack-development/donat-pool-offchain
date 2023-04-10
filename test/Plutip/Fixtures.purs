module Test.Plutip.Fixtures where

import Data.Tuple (Tuple)
import Contract.Test.Plutip (InitialUTxOsWithStakeKey, withStakeKey)
import Data.BigInt as BigInt
import Data.Tuple.Nested ((/\))
import Test.Plutip.Common (privateStakeKey)

distribution :: Tuple InitialUTxOsWithStakeKey InitialUTxOsWithStakeKey
distribution =
  withStakeKey privateStakeKey aliceUtxos
    /\ withStakeKey privateStakeKey bobUtxos
  where
  aliceUtxos =
    [ BigInt.fromInt 1_000_000_000
    , BigInt.fromInt 2_000_000_000
    ]
  bobUtxos =
    [ BigInt.fromInt 1_000_000_000
    , BigInt.fromInt 2_000_000_000
    ]
