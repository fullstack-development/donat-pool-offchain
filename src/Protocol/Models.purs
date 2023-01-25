module Protocol.Models where

import Contract.Prelude

import Contract.Value (CurrencySymbol, TokenName)
import Contract.Address (PaymentPubKeyHash)
import Contract.PlutusData

newtype PProtocol = PProtocol
  { managerPkh :: PaymentPubKeyHash
  , protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
  }

derive newtype instance Show PProtocol
derive instance Generic PProtocol _

instance
  HasPlutusSchema
    PProtocol
    ( "PProtocol"
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

derive newtype instance Eq PProtocol
derive newtype instance Ord PProtocol
instance ToData PProtocol where
  toData = genericToData

instance FromData PProtocol where
  fromData = genericFromData
