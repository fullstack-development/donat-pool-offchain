module Management.Proposal.UserData where

import Contract.Prelude

import Data.BigInt (BigInt)
import Proposal.Model (PProposalParameters)

newtype VoteData = VoteData
  { proposalThreadCurrency :: String
  , amount :: BigInt
  , for :: Boolean
  }

derive newtype instance Show VoteData
derive newtype instance Eq VoteData

newtype ProposalInfo = ProposalInfo
  { threadCurrency :: String
  , proposalData :: PProposalParameters
  , for :: BigInt
  , against :: BigInt
  }

derive newtype instance Show ProposalInfo
