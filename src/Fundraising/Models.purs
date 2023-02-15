module Fundraising.Models where

import Contract.Prelude

import Contract.Value (CurrencySymbol, TokenName)
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
import Protocol.Models (Protocol)

newtype Fundraising = Fundraising
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , verTokenName :: TokenName
  , threadTokenCurrency :: CurrencySymbol
  , threadTokenName :: TokenName
  }

derive newtype instance Show Fundraising
derive instance Generic Fundraising _
derive instance Newtype Fundraising _

instance
  HasPlutusSchema
    Fundraising
    ( "Fundraising"
        :=
          ( "protocol" := I Protocol
              :+ "verTokenCurrency"
              := I CurrencySymbol
              :+ "verTokenName"
              := I TokenName
              :+ "threadTokenCurrency"
              := I CurrencySymbol
              :+ "threadTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq Fundraising
derive newtype instance Ord Fundraising
instance ToData Fundraising where
  toData = genericToData

instance FromData Fundraising where
  fromData = genericFromData
