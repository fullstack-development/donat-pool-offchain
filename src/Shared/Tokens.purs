module Shared.Tokens where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Ctl.Internal.Plutus.Types.CurrencySymbol (CurrencySymbol)
import Ctl.Internal.Types.Scripts (MintingPolicy)
import Ctl.Internal.Types.TokenName (TokenName)
import Ctl.Internal.Types.Transaction (TransactionInput)
import MintingPolicy.NftMinting (mintingPolicy) as NFT
import MintingPolicy.ProposalMinting as Proposal
import MintingPolicy.VerTokenMinting (mintingPolicy) as VerToken
import Proposal.ProposalScript (proposalTokenName, proposalVerTokenName)
import Protocol.Models (Protocol)
import Ext.Contract.Value (mkCurrencySymbol)

-- TODO: refactoring: add all tokens
newtype GetTokenNames = GetTokenNames
  { proposalThread :: Contract TokenName
  , proposalVer :: Contract TokenName
  }

derive instance Generic GetTokenNames _
derive instance Newtype GetTokenNames _

getTokenNames :: GetTokenNames
getTokenNames = GetTokenNames
  { proposalThread: proposalTokenName
  , proposalVer: proposalVerTokenName
  }

type TokenInfo = (MintingPolicy /\ CurrencySymbol /\ TokenName)

createProposalThreadToken :: TransactionInput -> Contract TokenInfo
createProposalThreadToken policyRef =
  createToken (Proposal.proposalMintingPolicy policyRef) "Thread" (unwrap getTokenNames).proposalThread "Proposal"

createProposalVerToken :: Protocol -> Contract TokenInfo
createProposalVerToken protocol = createVerToken "Proposal" protocol (unwrap getTokenNames).proposalVer

createProposalVoteToken :: TransactionInput -> TokenName -> Contract TokenInfo
createProposalVoteToken policyRef tokenName =
  createToken (Proposal.proposalMintingPolicy policyRef) "Vote" (pure tokenName) "Proposal"

createThreadToken :: String -> TransactionInput -> Contract TokenName -> Contract TokenInfo
createThreadToken scriptName policyRef getTokenName =
  createToken (NFT.mintingPolicy policyRef) "Thread" getTokenName scriptName

createVerToken :: String -> Protocol -> Contract TokenName -> Contract TokenInfo
createVerToken scriptName protocol getTokenName = do
  createToken (VerToken.mintingPolicy protocol) "Verification" getTokenName scriptName

createToken :: Contract MintingPolicy -> String -> Contract TokenName -> String -> Contract TokenInfo
createToken makePolicy tokenType getTokenName scriptName = do
  nftMp /\ nftCs <- mkCurrencySymbol makePolicy
  nftTn <- getTokenName
  logInfo' $ scriptName <> " " <> tokenType <> " currency: " <> show nftCs <> ", token name: " <> show nftTn
  pure $ nftMp /\ nftCs /\ nftTn
