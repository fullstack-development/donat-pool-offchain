module Proposal.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, Z, genericToData)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Data.BigInt (BigInt)

type IsVoteFor = BigInt -- "against" = 0, "for" = 1
type Voter = PaymentPubKeyHash

data PProposalRedeemer =
  PVote IsVoteFor Voter

derive instance Generic PProposalRedeemer _

instance
  HasPlutusSchema
    PProposalRedeemer
    ( "PVote"
        := PNil
        @@ Z
        :+ PNil
    )

instance ToData PProposalRedeemer where
  toData = genericToData
