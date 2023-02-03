module Protocol.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Protocol.Models (PFundriseConfig, PProtocolConfig)

data PProtocolRedeemer
  = PUpdateProtocolConfig PProtocolConfig
  | PStartFundrise PFundriseConfig
  | PCloseProtocol

derive instance Generic PProtocolRedeemer _

instance
  HasPlutusSchema
    PProtocolRedeemer
    ( "PUpdateProtocolConfig" := PNil @@ Z
        :+ "PStartFundrise"
        := PNil
        @@ (S Z)
        :+ "PCloseProtocol"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData PProtocolRedeemer where
  toData = genericToData

