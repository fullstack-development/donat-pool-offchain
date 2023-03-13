module Protocol.Models where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, Address)
import Contract.PlutusData (class FromData, class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), I, PNil, Z, genericFromData, genericToData)
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)

newtype Protocol = Protocol
  { protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
  }

derive newtype instance Show Protocol
derive instance Generic Protocol _
derive instance Newtype Protocol _

instance
  HasPlutusSchema
    Protocol
    ( "Protocol"
        :=
          ( "protocolCurrency"
              := I CurrencySymbol
              :+ "protocolTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq Protocol
derive newtype instance Ord Protocol
instance ToData Protocol where
  toData = genericToData

instance FromData Protocol where
  fromData = genericFromData

newtype PProtocolConfig = PProtocolConfig
  { minAmount :: BigInt
  , maxAmount :: BigInt
  , minDuration :: BigInt
  , maxDuration :: BigInt
  , protocolFee :: BigInt
  }

derive newtype instance Show PProtocolConfig
derive instance Generic PProtocolConfig _
derive instance Newtype PProtocolConfig _

instance
  HasPlutusSchema
    PProtocolConfig
    ( "PProtocolConfig"
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
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq PProtocolConfig
derive newtype instance Ord PProtocolConfig
instance ToData PProtocolConfig where
  toData = genericToData

instance FromData PProtocolConfig where
  fromData = genericFromData

newtype PFundriseConfig = PFundriseConfig
  { scriptAddress :: Address
  , verCurrencySymbol :: CurrencySymbol
  , verTokenName :: TokenName
  , threadCurrencySymbol :: CurrencySymbol
  , threadTokenName :: TokenName
  , startedAt :: BigInt
  }

derive newtype instance Show PFundriseConfig
derive instance Generic PFundriseConfig _
derive instance Newtype PFundriseConfig _

instance
  HasPlutusSchema
    PFundriseConfig
    ( "PFundriseConfig"
        :=
          ( "scriptAddress" := I Address
              :+ "verCurrencySymbol"
              := I CurrencySymbol
              :+ "verTokenName"
              := I TokenName
              :+ "threadCurrencySymbol"
              := I CurrencySymbol
              :+ "threadTokenName"
              := I TokenName
              :+ "startedAt"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq PFundriseConfig
derive newtype instance Ord PFundriseConfig
instance ToData PFundriseConfig where
  toData = genericToData

instance FromData PFundriseConfig where
  fromData = genericFromData