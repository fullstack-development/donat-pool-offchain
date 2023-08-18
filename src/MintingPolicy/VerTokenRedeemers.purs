module MintingPolicy.VerTokenRedeemers where

import Contract.Prelude

import Contract.PlutusData
  ( genericToData
  , class ToData
  , class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  , S
  , Z
  )

import Contract.Value as Value

data PVerTokenRedeemer = 
  PMintVerToken Value.TokenName 
  | PBurnVerToken Value.TokenName
  | PMintProposalVerToken Value.TokenName

derive instance Generic PVerTokenRedeemer _

instance
  HasPlutusSchema
    PVerTokenRedeemer
    ( "PMintVerToken" := PNil @@ Z
        :+ "PBurnVerToken"
        := PNil @@ (S Z)
        :+ "PMintProposalVerToken"
        := PNil @@ (S (S Z))
        :+ PNil
    )

instance ToData PVerTokenRedeemer where
  toData = genericToData
