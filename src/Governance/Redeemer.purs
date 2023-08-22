module Governance.Redeemer where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, Z, genericToData)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Address (Address)
import Ctl.Internal.Types.Interval (POSIXTime)
import Proposal.Model (PProposalParameters)
type PProposalAddress = Address
type PProposalThreadCurrency = Value.CurrencySymbol
type PProposalVerCurrency = Value.CurrencySymbol
type PProposalStartedAt = POSIXTime

data PGovernanceRedeemer = PCreateProposal PProposalParameters PProposalAddress PProposalThreadCurrency PProposalVerCurrency PProposalStartedAt

derive instance Generic PGovernanceRedeemer _

instance
  HasPlutusSchema
    PGovernanceRedeemer
    ( "PCreateProposal"
        := PNil
        @@ Z
        :+ PNil
    )

instance ToData PGovernanceRedeemer where
  toData = genericToData
