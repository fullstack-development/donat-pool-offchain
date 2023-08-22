module Proposal.Create where

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
import Ctl.Internal.Scripts (mintingPolicyHash)
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Ext.Contract.Time (addTimes)
import Ext.Contract.Value (currencySymbolToString)
import Ext.Data.BigInt (eqBigInt)
import Governance.Datum (GovernanceDatum(..))
import Governance.Redeemer (PGovernanceRedeemer(..))
import Management.Proposal.UserData (ProposalInfo(..))
import MintingPolicy.ProposalMinting as Proposal
import MintingPolicy.VerTokenRedeemers as VerToken
import Proposal.Datum (PProposalDatum(..))
import Proposal.Model (PProposalParameters(..), mkProposal)
import Proposal.ProposalScript (getProposalValidatorHash)
import Protocol.Datum (PProtocolDatum(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.Duration (minutesToPosixTime)
import Shared.MinAda (minAdaValue)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.RunContract (runContractWithResult)
import Shared.Tokens (createProposalThreadToken, createProposalVerToken)
import Shared.Tx (completeTx, toDatum, toRedeemer)
import Shared.ScriptInfo (ScriptInfo(..), getGovernanceScriptInfo)

runCreateProposal :: (ProposalInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> PProposalParameters -> NetworkParams -> Effect Unit
runCreateProposal onComplete onError protocolData proposalParams networkParams = runContractWithResult onComplete onError networkParams $ contract protocolData proposalParams

contract :: ProtocolData -> PProposalParameters -> Contract ProposalInfo
contract protocolData proposalParams = do
  logInfo' "Running CreateProposal endpoint"
  protocol <- dataToProtocol protocolData
  ownCreds'@(OwnCredentials ownCreds) <- getOwnCreds

  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  ScriptInfo govScriptInfo <- getGovernanceScriptInfo protocol
  let (GovernanceDatum govDatum) = govScriptInfo.datum

  networkId <- getNetworkId
  let policyRef = ownCreds.nonCollateralORef
  threadMp /\ threadCs /\ threadTn <- createProposalThreadToken policyRef
  verMp /\ verCs /\ verTn <- createProposalVerToken protocol
  let verTokenPolicyHash = mintingPolicyHash verMp

  let proposal = mkProposal protocol verCs
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalAddress <- liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash
  let govRefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst govScriptInfo.refScriptUtxo) (snd govScriptInfo.refScriptUtxo)
  checkProposedValues proposalParams protocolInfo.pDatum
  now <- currentTime
  let deadline = addTimes now (minutesToPosixTime govDatum.duration)
  let
    proposalDatum = toDatum $ PProposalDatum
      { proposal: proposalParams
      , for: fromInt 0
      , against: fromInt 0
      , policyRef: policyRef
      , quorum: govDatum.quorum
      , initiator: (unwrap ownCreds.ownAddressWithNetworkTag).address
      , deadline: deadline
      , applied: fromInt 0
      }

    createProposalRedeemer = toRedeemer $ PCreateProposal proposalParams proposalAddress threadCs verCs now
    threadValue = Value.singleton threadCs threadTn one
    verValue = Value.singleton verCs verTn one
    proposalCost = Value.lovelaceValueOf govDatum.fee
    paymentToProposal =
      minAdaValue
        <> threadValue
        <> verValue
        <> proposalCost

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer
          (toRedeemer $ Proposal.PMintThreadToken threadTn)
          threadValue
        <> Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          verTokenPolicyHash
          (toRedeemer $ VerToken.PMintProposalVerToken verTn)
          verTn
          one
          protocolInfo.references.verTokenInput

        <> Constraints.mustSpendScriptOutputUsingScriptRef
          (fst govScriptInfo.utxo)
          createProposalRedeemer
          govRefScriptInput
        <> Constraints.mustPayToScriptAddress
          govScriptInfo.validatorHash
          (ScriptCredential govScriptInfo.validatorHash)
          (toDatum govScriptInfo.datum)
          Constraints.DatumInline
          govScriptInfo.value
        <> Constraints.mustPayToScriptAddress
          proposalValidatorHash
          (ScriptCredential proposalValidatorHash)
          (toDatum proposalDatum)
          Constraints.DatumInline
          paymentToProposal
        <> Constraints.mustBeSignedBy ownCreds.ownPkh
        <> Constraints.mustReferenceOutput (fst protocolInfo.pUtxo)
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.verTokenRef)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy threadMp
        <> Lookups.unspentOutputs ownCreds.ownUtxos
        <> Lookups.unspentOutputs govScriptInfo.utxos
        <> Lookups.unspentOutputs protocolInfo.pUtxos

  completeTx lookups constraints ownCreds'
  bech32Address <- addressToBech32 proposalAddress
  logInfo' $ "Current proposal address: " <> show bech32Address

  let
    proposalInfo = ProposalInfo
      { threadCurrency: currencySymbolToString threadCs
      , proposalData: proposalParams
      , for: fromInt 0
      , against: fromInt 0
      }
  logInfo' "Proposal created successfully"
  pure proposalInfo

checkProposedValues :: PProposalParameters -> PProtocolDatum -> Contract Unit
checkProposedValues (PProposalParameters propParams) (PProtocolDatum protocolDatum) = do
  when
    ( propParams.minAmount `eqBigInt` protocolDatum.minAmount
        && propParams.maxAmount `eqBigInt` protocolDatum.maxAmount
        && propParams.minDuration `eqBigInt` protocolDatum.minDuration
        && propParams.maxDuration `eqBigInt` protocolDatum.maxDuration
        &&
          propParams.protocolFee `eqBigInt` protocolDatum.protocolFee
    )
    $ liftEffect
    $ throw "Proposed values are already in protocol config"
