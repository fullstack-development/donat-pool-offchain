module Fundraising.Redeemer where

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Contract.Prelude
import Data.BigInt (BigInt)

data PFundraisingRedeemer
  = PDonate BigInt
  | PReceiveFunds

derive instance Generic PFundraisingRedeemer _

instance
  HasPlutusSchema
    PFundraisingRedeemer
    ( "PDonate" := PNil @@ Z
        :+ "PReceiveFunds"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData PFundraisingRedeemer where
  toData = genericToData
