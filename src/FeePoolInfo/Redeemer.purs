module FeePoolInfo.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, Z, genericToData)

data PFeePoolInfoRedeemer = PAddRecord

derive instance Generic PFeePoolInfoRedeemer _

instance
  HasPlutusSchema
    PFeePoolInfoRedeemer
    ( "PAddRecord" := PNil @@ Z
        :+ PNil
    )

instance ToData PFeePoolInfoRedeemer where
  toData = genericToData
