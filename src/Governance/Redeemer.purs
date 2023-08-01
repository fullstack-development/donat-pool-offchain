module Governance.Redeemer where

import Contract.Prelude
import Proposal.Model (PProposalParameters)

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Address (Address)

type PProposalAddress = Address
type PProposalThreadCurrency = Value.CurrencySymbol
type PProposalVerCurrency = Value.CurrencySymbol

data PGovernanceRedeemer = PCreateProposal PProposalParameters PProposalAddress PProposalThreadCurrency PProposalVerCurrency

derive instance Generic PGovernanceRedeemer _

instance
  HasPlutusSchema
    PGovernanceRedeemer
    ( "PCreateProposal"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData PGovernanceRedeemer where
  toData = genericToData
