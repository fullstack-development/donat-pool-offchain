module Proposal.Apply
  ( applyProposal
  , contract
  , processNoQuorum
  , rejectProposal
  )
  where

import Contract.Prelude
import Info.AllProposals (getAllProposalUtxos, getThreadCsByTn, hasReachedQuorum, isFinished, votedToApply)

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Cardano.Types.Value (CurrencySymbol)
import Ctl.Internal.Contract.WaitUntilSlot (currentTime)
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Ctl.Internal.Plutus.Types.Value (flattenNonAdaAssets)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.Lens (view)
import Data.Lens.Getter ((^.))
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Ext.Contract.Value (addAdaToValue, subtractAdaFromValue)
import Ext.Data.Boolean (booleanToBigInt)
import Proposal.Datum (PProposalDatum(..))
import Proposal.ProposalScript (proposalTokenName, proposalValidatorScript)
import Proposal.Redeemer (PProposalRedeemer(..))
import Protocol.Datum (PProtocolDatum(..), _managerAddress, _tokenOriginRef)
import Protocol.Models (PProtocolConfig(..), Protocol(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams, ProtocolData, dataToProtocol, getConfigFromProtocolDatum, mapToProtocolConfig)
import Shared.Config (mapToProtocolConfigParams, readDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds, getPkhSkhFromAddress)
import Shared.ScriptInfo (ScriptInfo(..))
import Shared.Tx (completeTx, toDatum, toRedeemer)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO, filterByToken)
import Ctl.Internal.Types.Interval (from)

-- runUpdateProtocol :: Effect Unit
-- runUpdateProtocol = do
--   protocolConfig <- readProtocolConfig
--   let protocolData = mapToProtocolData protocolConfig
--   donatPoolConfig <- readDonatPoolConfig
--   protocolConfigParams <- mapToProtocolConfigParams donatPoolConfig
--   launchAff_ $ runContract testnetKeyWalletConfig (contract protocolData protocolConfigParams)

contract :: ProtocolData -> Contract Unit
contract protocolData = do

  logInfo' "Running process proposals"
  protocol <- dataToProtocol protocolData

  now <- currentTime
  allProposalsInfo <- getAllProposalUtxos protocol
  let finished = Array.filter (isFinished now) allProposalsInfo
  let { yes: reachedQuorumList, no: notReachedQuorumList } = Array.partition hasReachedQuorum finished
  traverse_ (processNoQuorum protocol) notReachedQuorumList
  let { yes: applyList, no: rejectList } = Array.partition votedToApply reachedQuorumList
  traverse_ (applyProposal protocol) applyList
  traverse_ (rejectProposal protocol) rejectList
  logInfo' "Finished to process proposals"

rejectProposal :: Protocol -> ScriptInfo PProposalDatum -> Contract Unit
rejectProposal protocol proposalScriptInfo = do
  markAsProcessed protocol proposalScriptInfo true
  logInfo' "Proposal is rejected"

processNoQuorum :: Protocol -> ScriptInfo PProposalDatum -> Contract Unit
processNoQuorum protocol proposalScriptInfo = do
  markAsProcessed protocol proposalScriptInfo false
  logInfo' "Proposal is failed to archive a qourum, marked as processed"

markAsProcessed :: Protocol -> ScriptInfo PProposalDatum -> Boolean -> Contract Unit
markAsProcessed protocol (ScriptInfo proposalScriptInfo) isQuorumReached = do

  ProtocolScriptInfo protocolScriptInfo <- getProtocolScriptInfo protocol
  ownCreds@(OwnCredentials creds) <- getOwnCreds
  proposalThreadCs <- liftContractM "Impossible to get threadTokenCs from proposal UTXO" $ getThreadCsByTn proposalScriptInfo.utxo proposalScriptInfo.tokenName
  let PProposalDatum currentDatum = proposalScriptInfo.datum
  let currentValue = proposalScriptInfo.value

  now <- currentTime
  let redeemer = toRedeemer $ PRejectProposal proposalThreadCs (booleanToBigInt isQuorumReached)
  let newDatum = toDatum $ PProposalDatum
        { proposal: currentDatum.proposal
        , for: currentDatum.for
        , against: currentDatum.against
        , policyRef: currentDatum.policyRef
        , quorum: currentDatum.quorum
        , initiator: currentDatum.initiator
        , cost: currentDatum.cost
        , deadline: currentDatum.deadline
        , processed: fromInt 1
        }
  let newValue = 
        if isQuorumReached 
        then currentValue 
        else subtractAdaFromValue currentValue currentDatum.cost

  let payToManager = 
        if isQuorumReached 
        then Constraints.mustPayToPubKeyAddress creds.ownPkh creds.ownSkh (addAdaToValue currentValue currentDatum.cost)
        else mempty

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst proposalScriptInfo.utxo)
        redeemer
        proposalScriptInfo.refScriptInput
        <> Constraints.mustPayToScriptAddress
          proposalScriptInfo.validatorHash
          (ScriptCredential proposalScriptInfo.validatorHash)
          newDatum
          Constraints.DatumInline
          newValue
        <> Constraints.mustBeSignedBy creds.ownPkh
        
        <> Constraints.mustValidateIn (from now)
        <> Constraints.mustReferenceOutput (fst proposalScriptInfo.refScriptUtxo)
        <> Constraints.mustReferenceOutput (fst protocolScriptInfo.references.verTokenRef)
        <> payToManager

  let
    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs proposalScriptInfo.utxos
  

  completeTx lookups constraints ownCreds  
  logInfo' "Proposal is marked as processed"

applyProposal :: Protocol -> ScriptInfo PProposalDatum -> Contract Unit
applyProposal protocol (ScriptInfo proposalScriptInfo) = do

  --   (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  --   ownCreds@(OwnCredentials creds) <- getOwnCreds

  --   manager /\ _ <- getPkhSkhFromAddress $ view _managerAddress protocolInfo.pDatum
  --   when (manager /= creds.ownPkh) $ liftEffect $ throw "Current user doesn't have permissions to update protocol"

  --   let protocolConfig = mapToProtocolConfig protocolConfigParams
  --   let newDatum = makeDatum protocolInfo.pDatum protocolConfig
  --   logInfo' $ "New datum: " <> show newDatum
  --   let newPDatum = Datum $ toData $ newDatum

  --   let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  --   let
  --     constraints :: Constraints.TxConstraints Void Void
  --     constraints =
  --       Constraints.mustSpendScriptOutputUsingScriptRef
  --         (fst protocolInfo.pUtxo)
  --         updateProtocolRedeemer
  --         protocolInfo.references.pRefScriptInput
  --         <> Constraints.mustPayToScriptAddress
  --           protocolInfo.pValidatorHash
  --           (ScriptCredential protocolInfo.pValidatorHash)
  --           newPDatum
  --           Constraints.DatumInline
  --           protocolInfo.pValue
  --         <> Constraints.mustReferenceOutput (fst protocolInfo.references.pScriptRef)
  --         <> Constraints.mustBeSignedBy creds.ownPkh
  --   let
  --     lookups :: Lookups.ScriptLookups Void
  --     lookups =
  --       Lookups.unspentOutputs protocolInfo.pUtxos

  --   completeTx lookups constraints ownCreds

  --   pure $ getConfigFromProtocolDatum newDatum

  -- makeDatum ∷ PProtocolDatum -> PProtocolConfig → PProtocolDatum
  -- makeDatum currentDatum (PProtocolConfig { minAmount, maxAmount, minDuration, maxDuration, protocolFee }) =
  --   PProtocolDatum
  --     { minAmount: minAmount
  --     , maxAmount: maxAmount
  --     , minDuration: minDuration
  --     , maxDuration: maxDuration
  --     , protocolFee: protocolFee
  --     , managerAddress: view _managerAddress currentDatum
  --     , tokenOriginRef: view _tokenOriginRef currentDatum
  --     }
  logInfo' "Proposal is applied"