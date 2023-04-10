module Test.Plutip.Fixtures where

import Prelude
import Data.Tuple (Tuple)
import Contract.Test.Plutip (InitialUTxOsWithStakeKey, withStakeKey)
import Data.BigInt as BigInt
import Data.Tuple.Nested ((/\))
import Test.Plutip.Common (privateStakeKey)
import Protocol.Models (Protocol)
import Protocol.UserData (ProtocolConfigParams(..))
import Contract.Monad (Contract)
import Fundraising.UserData (FundraisingData(..))
import Shared.Helpers as Helpers
import Contract.Value as Value

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

incorrectFundraisingData :: Protocol -> Contract FundraisingData
incorrectFundraisingData protocol = do
  tn <- Helpers.runMkTokenName "FundraisingThreadToken"
  pure $ FundraisingData { protocol: protocol, frThreadTokenCurrency: Value.adaSymbol, frThreadTokenName: tn }

minDurationStartProtocolParams :: ProtocolConfigParams
minDurationStartProtocolParams = ProtocolConfigParams
  { minAmountParam: BigInt.fromInt 2_000_000
  , maxAmountParam: BigInt.fromInt 100_000_000
  , minDurationParam: BigInt.fromInt 1 -- minutes
  , maxDurationParam: BigInt.fromInt 250 -- minutes
  , protocolFeeParam: BigInt.fromInt 5 -- percentage
  }
