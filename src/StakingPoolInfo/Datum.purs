module StakingPoolInfo.Datum where

import Contract.AssocMap as Map
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Ext.Contract.Time (DayOfEpoch, Epoch)
import Prelude (class Eq, class Ord)

type DaoTokensAmt = BigInt

newtype PStakingPoolInfoDatum = PStakingPoolInfoDatum
  { epoch :: Epoch
  , history :: Map.Map DayOfEpoch DaoTokensAmt
  }

derive instance Generic PStakingPoolInfoDatum _
derive instance Newtype PStakingPoolInfoDatum _
derive newtype instance Show PStakingPoolInfoDatum
derive newtype instance Eq PStakingPoolInfoDatum
derive newtype instance Ord PStakingPoolInfoDatum

instance
  HasPlutusSchema
    PStakingPoolInfoDatum
    ( "PStakingPoolInfoDatum"
        :=
          ( "epoch" := I Epoch
              :+ "history"
              := I (Map.Map DayOfEpoch DaoTokensAmt)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PStakingPoolInfoDatum where
  toData = genericToData

instance FromData PStakingPoolInfoDatum where
  fromData = genericFromData
