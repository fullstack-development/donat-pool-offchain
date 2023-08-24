module Proposal.Vote where

import Contract.Prelude

import Contract.Address (addressToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Chain (currentTime)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (mkTxUnspentOut)
import Ctl.Internal.Types.Interval (from)
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Ext.Contract.Value (mkCurrencySymbol)
import Ext.Serialization.Token (deserializeCurrency)
import Governance.Datum (GovernanceDatum(..))
import Management.Proposal.UserData (VoteData(..))
import MintingPolicy.ProposalMinting (PProposalPolicyRedeemer(..))
import MintingPolicy.VerTokenMinting (mintingPolicy) as VerToken
import Proposal.Datum (PProposalDatum(..))
import Proposal.Model (mkProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalValidatorScript, proposalVerTokenName)
import Proposal.Redeemer (PProposalRedeemer(..))
import Proposal.VoteTokenName (mkVoteTokenName)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (minAdaValue)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.RunContract (runContractWithResult)
import Shared.ScriptInfo (ScriptInfo(..), getGovernanceScriptInfo, getProposalScriptInfo)
import Shared.ScriptRef (getUtxoWithRefScript)
import Shared.Tokens (createProposalVoteToken)
import Shared.Tx (completeTx, toDatum, toRedeemer)
import Shared.Utxo (checkTokenInUTxO)

runVote :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> VoteData -> NetworkParams -> Effect Unit
runVote onComplete onError protocolData voteData networkParams = runContractWithResult onComplete onError networkParams $ contract protocolData voteData

contract :: ProtocolData -> VoteData -> Contract Unit
contract protocolData (VoteData voteData) = do
  logInfo' "Running vote endpoint"
  protocol <- dataToProtocol protocolData
  ownCreds'@(OwnCredentials ownCreds) <- getOwnCreds

  networkId <- getNetworkId
  ScriptInfo govScriptInfo <- getGovernanceScriptInfo protocol
  let GovernanceDatum govDatum = govScriptInfo.datum

  let govTokensInWallet = Value.valueOf ownCreds.ownValue govDatum.govCurrency govDatum.govTokenName
  logInfo' $ "govTokensInWallet: " <> show govTokensInWallet
  when (govTokensInWallet < voteData.amount) $ liftEffect $ throw "Wallet doesn't have enough governance tokens"

  proposalCs <- deserializeCurrency voteData.proposalThreadCurrency
  proposalVerTn <- proposalVerTokenName
  _ /\ proposalVerTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  let proposal = mkProposal protocol proposalVerTokenCs

  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalAddress <- liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash
  ScriptInfo proposalScriptInfo <- getProposalScriptInfo proposal proposalCs

  unless (checkTokenInUTxO (Tuple proposalVerTokenCs proposalVerTn) proposalScriptInfo.utxo) $ liftEffect $ throw "VerificationToken not found in Proposal"
  let proposalDatum = unwrap proposalScriptInfo.datum
  when (proposalDatum.applied == fromInt 1) $ throw >>> liftEffect $ "Can't vote for the applied proposal"
  now <- currentTime
  when (now > proposalDatum.deadline) $ throw >>> liftEffect $ "Voting time is over"
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
  let voteTokenRedeemer = toRedeemer $ PMintVoteToken proposalVerTokenCs

  let govTokensValue = Value.singleton govDatum.govCurrency govDatum.govTokenName voteData.amount
  let paymentToProposal = proposalScriptInfo.value <> minAdaValue <> govTokensValue

  let voteTokensValue = Value.singleton voteCs voteTn one
  let paymentToVoter = minAdaValue <> voteTokensValue

  proposalValidator <- proposalValidatorScript proposal
  proposalRefScriptUtxo <- getUtxoWithRefScript (unwrap proposalValidator) proposalScriptInfo.utxos
  let proposalRefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst proposalRefScriptUtxo) (snd proposalRefScriptUtxo)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer
          voteTokenRedeemer
          voteTokensValue

        <> Constraints.mustSpendScriptOutputUsingScriptRef
          (fst proposalScriptInfo.utxo)
          voteRedeemer
          proposalRefScriptInput
        <> Constraints.mustPayToScriptAddress
          proposalScriptInfo.validatorHash
          (ScriptCredential proposalScriptInfo.validatorHash)
          newProposalDatum
          Constraints.DatumInline
          paymentToProposal
        <> Constraints.mustPayToPubKeyAddress ownCreds.ownPkh ownCreds.ownSkh paymentToVoter
        <> Constraints.mustBeSignedBy ownCreds.ownPkh
        <> Constraints.mustValidateIn votingTimeRange
        <> Constraints.mustReferenceOutput (fst proposalRefScriptUtxo)
        <> Constraints.mustReferenceOutput (fst govScriptInfo.utxo)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy voteMp
        <> Lookups.unspentOutputs ownCreds.ownUtxos
        <> Lookups.unspentOutputs proposalScriptInfo.utxos
        <> Lookups.unspentOutputs govScriptInfo.utxos

  completeTx lookups constraints ownCreds'
  bech32Address <- addressToBech32 proposalAddress
  logInfo' $ "Current proposal address: " <> show bech32Address
  logInfo' "Added vote successfully"
