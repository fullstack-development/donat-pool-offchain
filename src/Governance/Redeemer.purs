module Governance.Redeemer where

import Contract.Prelude
import Proposal.Model (PProposalParameters)
import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, Z, genericToData)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Address (Address)
import Data.BigInt (BigInt)

type PProposalAddress = Address
type PProposalThreadCurrency = Value.CurrencySymbol
type PProposalVerCurrency = Value.CurrencySymbol
type PProposalStartedAt = BigInt

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
