module FeePool.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Data.BigInt (BigInt)

type DepositAmount = BigInt

data PFeePoolRedeemer
  = PAddFundsWithCurrentEpoch DepositAmount
  | PAddFundsWithNewEpoch DepositAmount
  | PPayRewards -- not implemented for now

derive instance Generic PFeePoolRedeemer _

instance
  HasPlutusSchema
    PFeePoolRedeemer
    ( "PAddFundsWithCurrentEpoch" := PNil @@ Z
        :+ "PAddFundsWithNewEpoch"
        := PNil
        @@ (S Z)
        :+ "PPayRewards"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData PFeePoolRedeemer where
  toData = genericToData
