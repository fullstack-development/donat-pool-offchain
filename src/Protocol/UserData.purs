module Protocol.UserData where

import Contract.Prelude
import Contract.Monad (Contract, liftContractM)
import Protocol.Datum (PDurationLimits(..), PPoolSizeLimits(..), PProtocolConfig(..))
import Data.BigInt (fromInt)
import Shared.Helpers as Helpers

newtype ProtocolConfigParams = ProtocolConfigParams
  { minAmountParam :: Int
  , maxAmountParam :: Int
  , minDurationParam :: Int
  , maxDurationParam :: Int
  , protocolFeeParam :: Tuple Int Int
  }

derive newtype instance Show ProtocolConfigParams
derive newtype instance Eq ProtocolConfigParams

mapToProtocolConfig :: ProtocolConfigParams -> Contract () PProtocolConfig
mapToProtocolConfig (ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  let
    protocolSizeLimits = PPoolSizeLimits
      { minAmount: fromInt minAmountParam
      , maxAmount: fromInt maxAmountParam
      }
    protocolDurationLimits = PDurationLimits
      { minDuration: fromInt minDurationParam
      , maxDuration: fromInt maxDurationParam
      }
  fee <- liftContractM "Zero denominator error" $ Helpers.mkRational protocolFeeParam
  pure $ PProtocolConfig
    { protocolFee: fee
    , poolSizeLimits: protocolSizeLimits
    , durationLimits: protocolDurationLimits
    }