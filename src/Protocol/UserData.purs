module Protocol.UserData where

import Contract.Prelude
import Protocol.Models (PProtocolConfig(..))
import Protocol.Datum (PProtocolDatum(..))
import Data.BigInt (BigInt)

newtype ProtocolConfigParams = ProtocolConfigParams
  { minAmountParam :: BigInt
  , maxAmountParam :: BigInt
  , minDurationParam :: BigInt  -- minutes
  , maxDurationParam :: BigInt  -- minutes
  , protocolFeeParam :: BigInt -- percentage
  }

derive newtype instance Show ProtocolConfigParams
derive newtype instance Eq ProtocolConfigParams

mapToProtocolConfig :: ProtocolConfigParams -> PProtocolConfig
mapToProtocolConfig (ProtocolConfigParams configParams) = do
  PProtocolConfig
    { minAmount: configParams.minAmountParam
    , maxAmount: configParams.maxAmountParam
    , minDuration: configParams.minDurationParam
    , maxDuration: configParams.maxDurationParam
    , protocolFee: configParams.protocolFeeParam
    }

mapFromProtocolDatum :: PProtocolDatum -> ProtocolConfigParams
mapFromProtocolDatum (PProtocolDatum datum) =
  ProtocolConfigParams
    { minAmountParam: datum.minAmount
    , maxAmountParam: datum.maxAmount
    , minDurationParam: datum.minDuration
    , maxDurationParam: datum.maxDuration
    , protocolFeeParam: datum.protocolFee
    }
