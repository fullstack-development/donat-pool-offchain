module Protocol.Models where

import Contract.Value (CurrencySymbol, TokenName)
import Contract.Address (PaymentPubKeyHash)
import Contract.Prelude (class Generic)
import Prelude (class Eq, class Ord)
import Contract.PlutusData
import Data.Newtype (class Newtype)

newtype PProtocol = PProtocol {
    managerPkh :: PaymentPubKeyHash
  , protocolCurrency :: CurrencySymbol
  , protocolTokenName :: TokenName
}

derive instance Generic PProtocol _

instance
  HasPlutusSchema
    PProtocol
    ( "PProtocol" :=
              ( "managerPkh"  := I PaymentPubKeyHash
               :+ "protocolCurrency" := I CurrencySymbol
               :+ "protocolTokenName" := I TokenName
              :+ PNil)
           @@ Z
        :+ PNil
    )

derive instance Newtype PProtocol _
derive newtype instance Eq PProtocol
derive newtype instance Ord PProtocol
instance ToData PProtocol where
  toData = genericToData

instance FromData PProtocol where
  fromData = genericFromData