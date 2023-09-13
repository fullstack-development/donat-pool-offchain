module StakingPool.Models where

import Contract.Prelude

import Contract.PlutusData (class FromData, class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), I, PNil, Z, genericFromData, genericToData)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import Protocol.Models (Protocol)

newtype StakingPool = StakingPool
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , daoCurrency :: CurrencySymbol
  , daoTokenName :: TokenName
  }

derive newtype instance Show StakingPool
derive instance Generic StakingPool _
derive instance Newtype StakingPool _

instance
  HasPlutusSchema
    StakingPool
    ( "StakingPool"
        :=
          ( "protocol" := I Protocol
              :+ "verTokenCurrency"
              := I CurrencySymbol
              :+ "daoCurrency"
              := I CurrencySymbol
              :+ "daoTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq StakingPool
derive newtype instance Ord StakingPool

instance ToData StakingPool where
  toData = genericToData

instance FromData StakingPool where
  fromData = genericFromData
