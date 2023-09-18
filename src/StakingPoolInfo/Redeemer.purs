module StakingPoolInfo.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, Z, genericToData)

data PStakingPoolInfoRedeemer = PUpdateRecord

derive instance Generic PStakingPoolInfoRedeemer _

instance
  HasPlutusSchema
    PStakingPoolInfoRedeemer
    ( "PUpdateRecord" := PNil @@ Z
        :+ PNil
    )

instance ToData PStakingPoolInfoRedeemer where
  toData = genericToData
