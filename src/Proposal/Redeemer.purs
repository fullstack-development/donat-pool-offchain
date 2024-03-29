module Proposal.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, Z, genericToData)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Plutus.Types.Address (Address)
import Data.BigInt (BigInt)

type IsVoteFor = BigInt -- "against" = 0, "for" = 1
type PAmount = BigInt
type Voter = Address
type ProposalThreadCs = CurrencySymbol

data PProposalRedeemer =
  PVote IsVoteFor PAmount Voter ProposalThreadCs

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
