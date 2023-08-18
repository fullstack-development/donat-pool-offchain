module Proposal.Model where

import Contract.Prelude

import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Value as Value
import Ctl.Internal.FromData (class FromData, genericFromData)
import Ctl.Internal.ToData (class ToData)
import Data.BigInt (BigInt)
import Protocol.Models (Protocol(..))

newtype PProposal = PProposal
  { protocolCurrency :: Value.CurrencySymbol
  , verTokenCurrency :: Value.CurrencySymbol
  }

derive newtype instance Show PProposal
derive instance Generic PProposal _
derive instance Newtype PProposal _

instance
  HasPlutusSchema
    PProposal
    ( "PProposal"
        :=
          ( "protocolCurrency" := I Value.CurrencySymbol
              :+ "verTokenCurrency"
              := I Value.CurrencySymbol
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq PProposal
derive newtype instance Ord PProposal

instance ToData PProposal where
  toData = genericToData

instance FromData PProposal where
  fromData = genericFromData

newtype PProposalParameters = PProposalParameters
  { minAmount :: BigInt
  , maxAmount :: BigInt
  , minDuration :: BigInt
  , maxDuration :: BigInt
  , protocolFee :: BigInt
  }

derive newtype instance Show PProposalParameters
derive instance Generic PProposalParameters _
derive instance Newtype PProposalParameters _

instance
  HasPlutusSchema
    PProposalParameters
    ( "PProposalParameters"
        :=
          ( "minAmount" := I BigInt
              :+ "maxAmount"
              := I BigInt
              :+ "minDuration"
              := I BigInt
              :+ "maxDuration"
              := I BigInt
              :+ "protocolFee"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq PProposalParameters
derive newtype instance Ord PProposalParameters

instance ToData PProposalParameters where
  toData = genericToData

instance FromData PProposalParameters where
  fromData = genericFromData

mkProposal :: Protocol -> Value.CurrencySymbol -> PProposal
mkProposal (Protocol protocol) verCurrency = do
  PProposal
    { protocolCurrency: protocol.protocolCurrency
    , verTokenCurrency: verCurrency
    }
