module Protocol.Redeemer where

import Protocol.Datum (PProtocolConfig)
import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Contract.Prelude

data PProtocolRedeemer
  = PUpdateProtocolConfig PProtocolConfig
  | PCloseProtocol

derive instance Generic PProtocolRedeemer _

instance
  HasPlutusSchema
    PProtocolRedeemer
    ( "PUpdateProtocolConfig" := PNil @@ Z
        :+ "PCloseProtocol"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData PProtocolRedeemer where
  toData = genericToData
