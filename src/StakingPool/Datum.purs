module StakingPool.Datum where

import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.Newtype (class Newtype)
import Ext.Contract.Time (Epoch)
import Prelude (class Eq, class Ord)

newtype PStakingPoolDatum = PStakingPoolDatum { currentEpoch :: Epoch }

derive instance Generic PStakingPoolDatum _
derive instance Newtype PStakingPoolDatum _
derive newtype instance Show PStakingPoolDatum
derive newtype instance Eq PStakingPoolDatum
derive newtype instance Ord PStakingPoolDatum

instance
  HasPlutusSchema
    PStakingPoolDatum
    ( "PStakingPoolDatum"
        :=
          ( "currentEpoch" := I Epoch
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PStakingPoolDatum where
  toData = genericToData

instance FromData PStakingPoolDatum where
  fromData = genericFromData
