module MintingPolicy.NftRedeemer where

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

data PNftRedeemer = PMintNft Value.TokenName | PBurnNft Value.TokenName

derive instance Generic PNftRedeemer _

instance
  HasPlutusSchema
    PNftRedeemer
    ( "PMintNft" := PNil @@ Z
        :+ "PBurnNft"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData PNftRedeemer where
  toData = genericToData