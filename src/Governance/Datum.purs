module Governance.Datum where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)

newtype GovernanceDatum = GovernanceDatum
  { quorum :: BigInt
  , fee :: BigInt
  , govCurrency :: CurrencySymbol
  }

derive newtype instance Show GovernanceDatum
derive instance Generic GovernanceDatum _
derive instance Newtype GovernanceDatum _

instance
  HasPlutusSchema
    GovernanceDatum
    ( "GovernanceDatum"
        :=
          ( "quorum" := I BigInt
              :+ "fee"
              := I BigInt
              :+ "govCurrency"
              := I CurrencySymbol
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq GovernanceDatum
derive newtype instance Ord GovernanceDatum

instance ToData GovernanceDatum where
  toData = genericToData

instance FromData GovernanceDatum where
  fromData = genericFromData
