module Protocol.Datum where

import Ctl.Internal.FromData
import Contract.Address (Address)
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord, (<<<))
import Type.Proxy (Proxy(Proxy))

newtype PProtocolDatum = PProtocolDatum
  { minAmount :: BigInt
  , maxAmount :: BigInt
  , minDuration :: BigInt
  , maxDuration :: BigInt
  , protocolFee :: BigInt -- the percentage 
  , managerAddress :: Address
  , tokenOriginRef :: TransactionInput
  }

_minAmount :: Lens' PProtocolDatum BigInt
_minAmount = _Newtype <<< prop (Proxy :: Proxy "minAmount")

_maxAmount :: Lens' PProtocolDatum BigInt
_maxAmount = _Newtype <<< prop (Proxy :: Proxy "maxAmount")

_minDuration :: Lens' PProtocolDatum BigInt
_minDuration = _Newtype <<< prop (Proxy :: Proxy "minDuration")

_maxDuration :: Lens' PProtocolDatum BigInt
_maxDuration = _Newtype <<< prop (Proxy :: Proxy "maxDuration")

_protocolFee :: Lens' PProtocolDatum BigInt
_protocolFee = _Newtype <<< prop (Proxy :: Proxy "protocolFee")

_managerAddress :: Lens' PProtocolDatum Address
_managerAddress = _Newtype <<< prop (Proxy :: Proxy "managerAddress")

_tokenOriginRef :: Lens' PProtocolDatum TransactionInput
_tokenOriginRef = _Newtype <<< prop (Proxy :: Proxy "tokenOriginRef")

derive instance Generic PProtocolDatum _
derive instance Newtype PProtocolDatum _
derive newtype instance Show PProtocolDatum
derive newtype instance Eq PProtocolDatum
derive newtype instance Ord PProtocolDatum

instance
  HasPlutusSchema
    PProtocolDatum
    ( "PProtocolDatum"
        :=
          ( "minAmount" := I BigInt
              :+ "maxAmount"
              := I BigInt
              :+ "minDuration"
              := I BigInt
              :+ "maxDuration"
              := I BigInt
              :+ "protocolFee"
              := I BigInt
              :+ "managerAddress"
              := I Address
              :+ "tokenOriginRef"
              := I TransactionInput
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PProtocolDatum where
  toData = genericToData

instance FromData PProtocolDatum where
  fromData = genericFromData
