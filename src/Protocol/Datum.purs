module Protocol.Datum where

import Contract.PlutusData
  ( class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , Z
  , genericToData
  )
import Contract.Prelude (class Generic)
import Data.BigInt (BigInt)
import Data.Rational (Ratio)
import Ctl.Internal.Types.UsedTxOuts (TxOutRefCache)
import Contract.Address (PaymentPubKeyHash)
import Contract.Value (CurrencySymbol, TokenName)

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

instance ToData PPoolSizeLimits where
  toData = genericToData

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

instance ToData PDurationLimits where
  toData = genericToData

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

instance ToData PProtocolConfig where
  toData = genericToData

newtype PProtocolConstants = PProtocolConstants
  { managerPkh :: PaymentPubKeyHash
  , tokenOriginRef :: TxOutRefCache
  , protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
  }

derive instance Generic PProtocolConstants _

newtype PProtocolDatum = PProtocolDatum
  { protocolConstants :: PProtocolConstants
  , protocolConfig :: PProtocolConfig
  }
