module FeePoolInfo.Datum where

import Contract.AssocMap as Map
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Ext.Contract.Time (DayOfEpoch, Epoch)
import Prelude (class Eq, class Ord)

type FeeAmount = BigInt

newtype PFeePoolInfoDatum = PFeePoolInfoDatum
  { epoch :: Epoch
  , fee :: Map.Map DayOfEpoch FeeAmount
  }

derive instance Generic PFeePoolInfoDatum _
derive instance Newtype PFeePoolInfoDatum _
derive newtype instance Show PFeePoolInfoDatum
derive newtype instance Eq PFeePoolInfoDatum
derive newtype instance Ord PFeePoolInfoDatum

instance
  HasPlutusSchema
    PFeePoolInfoDatum
    ( "PFeePoolInfoDatum"
        :=
          ( "epoch" := I Epoch
              :+ "fee"
              := I (Map.Map DayOfEpoch FeeAmount)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PFeePoolInfoDatum where
  toData = genericToData

instance FromData PFeePoolInfoDatum where
  fromData = genericFromData
