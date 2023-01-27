module Protocol.Datum where

import Ctl.Internal.FromData
import Contract.Address (PaymentPubKeyHash)
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Contract.Value (CurrencySymbol, TokenName)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Rational (Ratio)
import Prelude (class Eq, class Ord, (<<<))
import Type.Proxy (Proxy(Proxy))

newtype PPoolSizeLimits = PPoolSizeLimits
  { minAmount :: BigInt
  , maxAmount :: BigInt
  }

derive instance Generic PPoolSizeLimits _

instance
  HasPlutusSchema
    PPoolSizeLimits
    ( "PPoolSizeLimits"
        :=
          ( "minAmount" := I BigInt
              :+ "maxAmount"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Show PPoolSizeLimits
derive newtype instance Eq PPoolSizeLimits
derive newtype instance Ord PPoolSizeLimits
instance ToData PPoolSizeLimits where
  toData = genericToData

instance FromData PPoolSizeLimits where
  fromData = genericFromData

newtype PDurationLimits = PDurationLimits
  { minDuration :: BigInt
  , maxDuration :: BigInt
  }

derive instance Generic PDurationLimits _

instance
  HasPlutusSchema
    PDurationLimits
    ( "PDurationLimits"
        :=
          ( "minDuration" := I BigInt
              :+ "maxDuration"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Show PDurationLimits
derive newtype instance Eq PDurationLimits
derive newtype instance Ord PDurationLimits
instance ToData PDurationLimits where
  toData = genericToData

instance FromData PDurationLimits where
  fromData = genericFromData

newtype PProtocolConfig = PProtocolConfig
  { protocolFee :: Ratio BigInt
  , poolSizeLimits :: PPoolSizeLimits
  , durationLimits :: PDurationLimits
  }

derive instance Generic PProtocolConfig _

instance
  HasPlutusSchema
    PProtocolConfig
    ( "PProtocolConfig"
        :=
          ( "protocolFee" := I (Ratio BigInt)
              :+ "poolSizeLimits"
              := I PPoolSizeLimits
              :+ "durationLimits"
              := I PDurationLimits
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Show PProtocolConfig
derive newtype instance Eq PProtocolConfig
derive newtype instance Ord PProtocolConfig
instance ToData PProtocolConfig where
  toData = genericToData

instance FromData PProtocolConfig where
  fromData = genericFromData

newtype PProtocolConstants = PProtocolConstants
  { managerPkh :: PaymentPubKeyHash
  , tokenOrigin :: TransactionInput
  , protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
  }

derive instance Generic PProtocolConstants _
derive newtype instance Show PProtocolConstants
derive newtype instance Eq PProtocolConstants
derive newtype instance Ord PProtocolConstants

instance
  HasPlutusSchema
    PProtocolConstants
    ( "PProtocolConstants"
        :=
          ( "managerPkh" := I PaymentPubKeyHash
              :+ "tokenOrigin"
              := I TransactionInput
              :+ "protocolCurrency"
              := I CurrencySymbol
              :+ "protocolTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PProtocolConstants where
  toData = genericToData

instance FromData PProtocolConstants where
  fromData = genericFromData

newtype PProtocolDatum = PProtocolDatum
  { protocolConstants :: PProtocolConstants
  , protocolConfig :: PProtocolConfig
  }

_protocolConstants :: Lens' PProtocolDatum PProtocolConstants
_protocolConstants = _Newtype <<< prop (Proxy :: Proxy "protocolConstants")

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
          ( "protocolConstants" := I PProtocolConstants
              :+ "protocolConfig"
              := I PProtocolConfig
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PProtocolDatum where
  toData = genericToData

instance FromData PProtocolDatum where
  fromData = genericFromData
