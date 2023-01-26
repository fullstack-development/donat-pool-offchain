module Protocol.UserData where

import Contract.Prelude

newtype ProtocolConfigParams = ProtocolConfigParams
  { minAmountParam :: Int
  , maxAmountParam :: Int
  , minDurationParam :: Int
  , maxDurationParam :: Int
  , protocolFeeParam :: Tuple Int Int
  }

derive newtype instance Show ProtocolConfigParams
derive newtype instance Eq ProtocolConfigParams
