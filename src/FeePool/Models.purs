module FeePool.Models where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), I, PNil, Z, genericFromData, genericToData)
import Contract.Value (CurrencySymbol)
import Data.Newtype (class Newtype)
import Ext.Contract.Value (mkCurrencySymbol)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.Models (Protocol)

newtype FeePool = FeePool
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  }

derive newtype instance Show FeePool
derive instance Generic FeePool _
derive instance Newtype FeePool _

instance
  HasPlutusSchema
    FeePool
    ( "FeePool"
        :=
          ( "protocol" := I Protocol
              :+ "verTokenCurrency"
              := I CurrencySymbol
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq FeePool
derive newtype instance Ord FeePool

instance ToData FeePool where
  toData = genericToData

instance FromData FeePool where
  fromData = genericFromData

mkFeePoolFromProtocol :: Protocol -> Contract FeePool
mkFeePoolFromProtocol protocol = do
  _ /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  pure $ FeePool { protocol: protocol, verTokenCurrency: verTokenCs }
