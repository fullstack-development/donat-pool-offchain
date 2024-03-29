-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main, Contracts) where

import Contract.Prelude

import Fundraising.Create as CreateFundraising
import Fundraising.Donate as Donate
import Fundraising.ReceiveFunds as ReceiveFunds
import Fundraising.UserData (CreateFundraisingParams, FundraisingData)
import Governance.MintGovernanceTokens as GovTokens
import Info.AllFundraisings as AllFundraisings
import Info.AppInfo as AppInfo
import Info.UserData (AppInfo, FundraisingInfo)
import Info.UserRelatedFundraisings as UserRelatedFundraisings
import Management.Proposal.UserData (ProposalInfo, VoteData)
import Proposal.Create as CreateProposal
import Proposal.Model (PProposalParameters)
import Proposal.Vote as Vote
import Protocol.UserData (ProtocolData)
import Shared.NetworkData (NetworkParams)

data Contracts = Contracts
  { getAppInfo :: (AppInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
  , createFundraising :: (FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> CreateFundraisingParams -> Effect Unit
  , donate :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> FundraisingData -> Int -> Effect Unit
  , receiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> FundraisingData -> Effect Unit
  , getAllFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
  , getUserRelatedFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
  , mintGovernanceTokens :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> NetworkParams -> Effect Unit
  , createProposal :: (ProposalInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> PProposalParameters -> NetworkParams -> Effect Unit
  , vote :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> VoteData -> NetworkParams -> Effect Unit
  }

main :: Contracts
main = Contracts
  { getAppInfo: AppInfo.runGetAppInfo
  , createFundraising: CreateFundraising.runCreateFundraising
  , donate: Donate.runDonate
  , receiveFunds: ReceiveFunds.runReceiveFunds
  , getAllFundraisings: AllFundraisings.runGetAllFundraisings
  , getUserRelatedFundraisings: UserRelatedFundraisings.runGetUserRelatedFundraisings
  , mintGovernanceTokens: GovTokens.runMintGovernanceTokens
  , createProposal: CreateProposal.runCreateProposal
  , vote: Vote.runVote
  }

