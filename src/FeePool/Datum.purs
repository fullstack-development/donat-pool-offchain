module FeePool.Datum where

import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.Newtype (class Newtype)
import Ext.Contract.Time (Epoch)
import Prelude (class Eq, class Ord)

newtype PFeePoolDatum = PFeePoolDatum { currentEpoch :: Epoch }

derive instance Generic PFeePoolDatum _
derive instance Newtype PFeePoolDatum _
derive newtype instance Show PFeePoolDatum
derive newtype instance Eq PFeePoolDatum
derive newtype instance Ord PFeePoolDatum

instance
  HasPlutusSchema
    PFeePoolDatum
    ( "PFeePoolDatum"
        :=
          ( "currentEpoch" := I Epoch
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PFeePoolDatum where
  toData = genericToData

instance FromData PFeePoolDatum where
  fromData = genericFromData
