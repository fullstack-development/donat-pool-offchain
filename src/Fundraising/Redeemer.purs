module Fundraising.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Data.BigInt (BigInt)
import Contract.Value (CurrencySymbol, TokenName)

data PFundraisingRedeemer
  = PDonate CurrencySymbol TokenName BigInt
  | PReceiveFundsCurrentEpoch CurrencySymbol TokenName
  | PReceiveFundsNewEpoch CurrencySymbol TokenName
  | PReceiveFundsWithoutFee CurrencySymbol TokenName

derive instance Generic PFundraisingRedeemer _

instance
  HasPlutusSchema
    PFundraisingRedeemer
    ( "PDonate" := PNil @@ Z
        :+ "PReceiveFundsCurrentEpoch"
        := PNil
        @@ (S Z)
        :+ "PReceiveFundsNewEpoch"
        := PNil
        @@ (S (S Z))
        :+ "PReceiveFundsWithoutFee"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

instance ToData PFundraisingRedeemer where
  toData = genericToData
