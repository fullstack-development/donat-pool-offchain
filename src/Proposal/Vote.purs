module Proposal.Vote where

import Contract.Prelude
import Ext.Contract.Value  (mkCurrencySymbol)
import Proposal.Redeemer (PProposalRedeemer(..))
import Contract.Address (addressToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (mkTxUnspentOut)
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Ext.Seriaization.Token (deserializeCurrency)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Governance.GovernanceScriptInfo (GovernanceScriptInfo(..), getGovernanceScriptInfo)
import Management.Proposal.UserData (VoteData(..))
import MintingPolicy.ProposalMinting (PProposalPolicyRedeemer(..))
import MintingPolicy.VerTokenMinting (mintingPolicy) as VerToken
import Proposal.Model (mkProposal)
import Proposal.ProposalScriptInfo (ProposalScriptInfo(..), getProposalScriptInfo)
import Proposal.VoteTokenName (mkVoteTokenName)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (minAdaValue)
import Shared.NetworkData (NetworkParams)
import Shared.RunContract (runContractWithResult)
import Shared.ScriptRef (getUtxoWithRefScript)
import Proposal.Datum (PProposalDatum(..))
import Proposal.ProposalScript (getProposalValidatorHash, proposalValidatorScript, proposalVerTokenName)
import Shared.Tokens (createProposalVoteToken)
import Shared.Tx (completeTx, toDatum, toRedeemer)
import Shared.Utxo (checkTokenInUTxO)
import Contract.Chain (currentTime)
import Ctl.Internal.Types.Interval (from)


runVote :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> VoteData -> NetworkParams -> Effect Unit
runVote onComplete onError protocolData voteData networkParams = runContractWithResult onComplete onError networkParams $ contract protocolData voteData

contract :: ProtocolData -> VoteData -> Contract Unit
contract protocolData (VoteData voteData) = do
  logInfo' "Running vote endpoint"
  protocol <- dataToProtocol protocolData
  ownCreds'@(OwnCredentials ownCreds) <- getOwnCreds
  networkId <- getNetworkId

  GovernanceScriptInfo govScriptInfo <- getGovernanceScriptInfo protocol
  let govDatum = unwrap govScriptInfo.govDatum

  proposalCs <- deserializeCurrency voteData.proposalThreadCurrency
  proposalVerTn <- proposalVerTokenName
  _ /\ proposalVerTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  let proposal = mkProposal protocol proposalVerTokenCs

  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalAddress <- liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash
  ProposalScriptInfo proposalScriptInfo <- getProposalScriptInfo proposalCs proposal

  unless (checkTokenInUTxO (Tuple proposalVerTokenCs proposalVerTn) proposalScriptInfo.prUtxo) $ liftEffect $ throw "VerificationToken not found in Proposal"
  let proposalDatum = unwrap proposalScriptInfo.prDatum

  now <- currentTime
  when (now > proposalDatum.deadline) $ throw >>> liftEffect $ "voting time is over"
  let votingTimeRange = from now

  let
    newProposalDatum = toDatum $ PProposalDatum
      { proposal: proposalDatum.proposal
      , for: if voteData.for then (proposalDatum.for + voteData.amount) else proposalDatum.for
      , against: if voteData.for then proposalDatum.against else (proposalDatum.against + voteData.amount)
      , policyRef: proposalDatum.policyRef
      , quorum: proposalDatum.quorum
      , initiator: proposalDatum.initiator
      , deadline: proposalDatum.deadline
      , applied: proposalDatum.applied
      }
  let isVoteFor = if voteData.for then (fromInt 1) else (fromInt 0)
  let voter = (unwrap ownCreds.ownAddressWithNetworkTag).address
  let voteRedeemer = toRedeemer $ PVote isVoteFor voteData.amount voter proposalCs

  voteTokenName <- liftContractM "Impossible to make Vote token name" $ mkVoteTokenName isVoteFor voteData.amount
  voteMp /\ voteCs /\ voteTn <- createProposalVoteToken proposalDatum.policyRef voteTokenName
  when (voteCs /= proposalCs) $ liftEffect $ throw "Unexpected vote token currency"
  let voteTokenRedeemer = toRedeemer $ PMintVoteToken proposalVerTokenCs isVoteFor voteData.amount

  let govTokensValue = Value.singleton govDatum.govCurrency govDatum.govTokenName voteData.amount
  let paymentToProposal = proposalScriptInfo.prValue <> minAdaValue <> govTokensValue

  let voteTokensValue = Value.singleton voteCs voteTn one
  let paymentToVoter = minAdaValue <> voteTokensValue

  proposalValidator <- proposalValidatorScript proposal
  proposalRefScriptUtxo <- getUtxoWithRefScript (unwrap proposalValidator) proposalScriptInfo.prUtxos
  let proposalRefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst proposalRefScriptUtxo) (snd proposalRefScriptUtxo)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer
          voteTokenRedeemer
          voteTokensValue

        <> Constraints.mustSpendScriptOutputUsingScriptRef
          (fst proposalScriptInfo.prUtxo)
          voteRedeemer
          proposalRefScriptInput
        <> Constraints.mustPayToScriptAddress
          proposalScriptInfo.prValidatorHash
          (ScriptCredential proposalScriptInfo.prValidatorHash)
          newProposalDatum
          Constraints.DatumInline
          paymentToProposal
        <> Constraints.mustPayToPubKeyAddress ownCreds.ownPkh ownCreds.ownSkh paymentToVoter
        <> Constraints.mustBeSignedBy ownCreds.ownPkh
        <> Constraints.mustValidateIn votingTimeRange
        <> Constraints.mustReferenceOutput (fst proposalRefScriptUtxo)
        <> Constraints.mustReferenceOutput (fst govScriptInfo.govUtxo)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy voteMp
        <> Lookups.unspentOutputs ownCreds.ownUtxos
        <> Lookups.unspentOutputs proposalScriptInfo.prUtxos
        <> Lookups.unspentOutputs govScriptInfo.govUtxos

  completeTx lookups constraints ownCreds'
  bech32Address <- addressToBech32 proposalAddress
  logInfo' $ "Current proposal address: " <> show bech32Address
  logInfo' "Added vote successfully"
