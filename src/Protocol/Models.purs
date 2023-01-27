module Protocol.Models where

import Contract.Prelude

import Contract.Value (CurrencySymbol, TokenName)
import Contract.Address (PaymentPubKeyHash)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , Z
  , genericFromData
  , genericToData
  )
import Data.Newtype (class Newtype)

newtype Protocol = Protocol
  { managerPkh :: PaymentPubKeyHash
  , protocolCurrency :: CurrencySymbol
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
          ( "managerPkh" := I PaymentPubKeyHash
              :+ "protocolCurrency"
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
