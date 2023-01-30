module Protocol.UserData where

import Contract.Prelude
import Contract.Monad (Contract)
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

mapToProtocolConfig :: ProtocolConfigParams -> Contract () PProtocolConfig
mapToProtocolConfig (ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  pure $ PProtocolConfig
    { minAmount: fromInt minAmountParam
    , maxAmount: fromInt maxAmountParam
    , minDuration: fromInt minDurationParam
    , maxDuration: fromInt maxDurationParam
    , protocolFee: fromInt protocolFeeParam
    }