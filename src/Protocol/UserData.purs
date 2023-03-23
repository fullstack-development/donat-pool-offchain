module Protocol.UserData where

import Contract.Prelude
import Protocol.Models (PProtocolConfig(..))
import Protocol.Datum (PProtocolDatum(..))
import Data.BigInt (BigInt)
import Shared.Duration (Duration, durationToMinutes, minutesToDuration)

newtype ProtocolConfigParams = ProtocolConfigParams
  { minAmountParam :: BigInt
  , maxAmountParam :: BigInt
  , minDurationParam :: Duration
  , maxDurationParam :: Duration
  , protocolFeeParam :: BigInt -- percentage
  }

derive newtype instance Show ProtocolConfigParams
derive newtype instance Eq ProtocolConfigParams

mapToProtocolConfig :: ProtocolConfigParams -> PProtocolConfig
mapToProtocolConfig (ProtocolConfigParams configParams) = do
  PProtocolConfig
    { minAmount: configParams.minAmountParam
    , maxAmount: configParams.maxAmountParam
    , minDuration: durationToMinutes configParams.minDurationParam
    , maxDuration: durationToMinutes configParams.maxDurationParam
    , protocolFee: configParams.protocolFeeParam
    }

mapFromProtocolDatum :: PProtocolDatum -> ProtocolConfigParams
mapFromProtocolDatum (PProtocolDatum datum) =
  ProtocolConfigParams
    { minAmountParam: datum.minAmount
    , maxAmountParam: datum.maxAmount
    , minDurationParam: minutesToDuration datum.minDuration
    , maxDurationParam: minutesToDuration datum.maxDuration
    , protocolFeeParam: datum.protocolFee
    }
