module Protocol.UserData where

import Contract.Prelude
import Protocol.Models (PProtocolConfig(..))
import Data.BigInt (fromInt)

newtype ProtocolConfigParams = ProtocolConfigParams
  { minAmountParam :: Int
  , maxAmountParam :: Int
  , minDurationParam :: Int
  , maxDurationParam :: Int
  , protocolFeeParam :: Int -- percentage
  }

derive newtype instance Show ProtocolConfigParams
derive newtype instance Eq ProtocolConfigParams

mapToProtocolConfig :: ProtocolConfigParams -> PProtocolConfig
mapToProtocolConfig (ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  PProtocolConfig
    { minAmount: fromInt minAmountParam
    , maxAmount: fromInt maxAmountParam
    , minDuration: fromInt minDurationParam
    , maxDuration: fromInt maxDurationParam
    , protocolFee: fromInt protocolFeeParam
    }

-- mapFromProtocolConfig :: PProtocolConfig -> ProtocolConfigParams
-- mapFromProtocolConfig (PProtocolConfig config) =
--   ProtocolConfigParams {
--     minAmountParam: toInt config.minAmount,
--     maxAmountParam: config.maxAmount,
--     minDurationParam: config.minDuration,
--     maxDurationParam: config.maxDuration,
--     protocolFeeParam: config.protocolFee
--   }