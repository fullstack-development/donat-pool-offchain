module Fundraising.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Data.BigInt (BigInt)
import Contract.Value (CurrencySymbol, TokenName)

data PFundraisingRedeemer
  = PDonate CurrencySymbol TokenName BigInt
  | PReceiveFunds CurrencySymbol TokenName

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
