module Proposal.Create where

import Contract.Prelude

import Contract.Address (addressToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Chain (currentTime)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime(..))
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (mkTxUnspentOut)
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Ext.Contract.Time (addTimes)
import Ext.Contract.Value (currencySymbolToString)
import Ext.Data.BigInt (eqBigInt)
import Governance.Datum (GovernanceDatum(..))
import Governance.GovernanceScriptInfo (GovernanceScriptInfo(..), getGovernanceScriptInfo)
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

runCreateProposal :: (ProposalInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> PProposalParameters -> NetworkParams -> Effect Unit
runCreateProposal onComplete onError protocolData proposalParams networkParams = runContractWithResult onComplete onError networkParams $ contract protocolData proposalParams

contract :: ProtocolData -> PProposalParameters -> Contract ProposalInfo
contract protocolData proposalParams = do
  logInfo' "Running CreateProposal endpoint"
  protocol <- dataToProtocol protocolData
  ownCreds'@(OwnCredentials ownCreds) <- getOwnCreds

  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  let refs = protocolInfo.references
  GovernanceScriptInfo govScriptInfo <- getGovernanceScriptInfo protocol
  let (GovernanceDatum govDatum) = govScriptInfo.govDatum

  networkId <- getNetworkId
  let policyRef = ownCreds.nonCollateralORef
  threadMp /\ threadCs /\ threadTn <- createProposalThreadToken policyRef
  verMp /\ verCs /\ verTn <- createProposalVerToken protocol
  let proposal = mkProposal protocol verCs
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalAddress <- liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash
  let govRefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst govScriptInfo.govRefScriptUtxo) (snd govScriptInfo.govRefScriptUtxo)

  checkProposedValues proposalParams protocolInfo.pDatum
  now@(POSIXTime now') <- currentTime
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

    createProposalRedeemer = toRedeemer $ PCreateProposal proposalParams proposalAddress threadCs verCs now'
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
        <> Constraints.mustMintValueWithRedeemer
          (toRedeemer $ VerToken.PMintProposalVerToken verTn)
          verValue

        <> Constraints.mustSpendScriptOutputUsingScriptRef
          (fst govScriptInfo.govUtxo)
          createProposalRedeemer
          govRefScriptInput
        <> Constraints.mustPayToScriptAddress
          govScriptInfo.govValidatorHash
          (ScriptCredential govScriptInfo.govValidatorHash)
          (toDatum govScriptInfo.govDatum)
          Constraints.DatumInline
          govScriptInfo.govValue

        <> Constraints.mustPayToScriptAddress
          proposalValidatorHash
          (ScriptCredential proposalValidatorHash)
          (toDatum proposalDatum)
          Constraints.DatumInline
          paymentToProposal

        <> Constraints.mustBeSignedBy ownCreds.ownPkh
        <> Constraints.mustReferenceOutput (fst govScriptInfo.govRefScriptUtxo)
        <> Constraints.mustReferenceOutput (fst refs.pScriptRef)

    -- <> Constraints.mustReferenceOutput (fst refs.verTokenRef)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy threadMp
        <> Lookups.mintingPolicy verMp
        <> Lookups.unspentOutputs ownCreds.ownUtxos
        <> Lookups.unspentOutputs govScriptInfo.govUtxos
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
