module Test.Plutip.Fixtures where

import Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip (InitialUTxOsWithStakeKey, withStakeKey)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Fundraising.UserData (FundraisingData(..))
import Protocol.UserData (ProtocolConfigParams(..), ProtocolData, dataToProtocol)
import Shared.Helpers as Helpers
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

incorrectFundraisingData :: ProtocolData -> Contract FundraisingData
incorrectFundraisingData protocolData = do
  protocol <- dataToProtocol protocolData
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
