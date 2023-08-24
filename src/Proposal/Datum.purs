module Proposal.Datum where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Interval (POSIXTime)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Proposal.Model (PProposalParameters)

newtype PProposalDatum = PProposalDatum
  { proposal :: PProposalParameters
  , for :: BigInt
  , against :: BigInt
  , policyRef :: TransactionInput
  , quorum :: BigInt
  , initiator :: Address
  , deadline :: POSIXTime
  , applied :: BigInt -- 0 or 1
  }

derive instance Generic PProposalDatum _
derive instance Newtype PProposalDatum _

instance
  HasPlutusSchema
    PProposalDatum
    ( "PProposalDatum"
        :=
          ( "proposal" := I PProposalParameters
              :+ "for"
              := I BigInt
              :+ "against"
              := I BigInt
              :+ "policyRef"
              := I TransactionInput
              :+ "quorum"
              := I BigInt
              :+ "initiator"
              := I Address
              :+ "deadline"
              := I POSIXTime
              :+ "applied"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PProposalDatum where
  toData = genericToData

instance FromData PProposalDatum where
  fromData = genericFromData
