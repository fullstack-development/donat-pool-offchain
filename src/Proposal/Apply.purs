module Proposal.Apply where

import Contract.Prelude
import Info.AllProposals

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Ctl.Internal.Contract.WaitUntilSlot (currentTime)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Array (filter, partition) as Array
import Data.Lens (view)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Protocol.Datum (PProtocolDatum(..), _managerAddress, _tokenOriginRef)
import Protocol.Models (PProtocolConfig(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams, ProtocolData, dataToProtocol, getConfigFromProtocolDatum, mapToProtocolConfig)
import Shared.Config (mapToProtocolConfigParams, readDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds, getPkhSkhFromAddress)
import Shared.Tx (completeTx)
import Shared.Utxo (UtxoTuple, filterByToken)

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
  proposalsUtxos <- getAllProposalUtxos protocol
  let finished = Array.filter (isFinished now) proposalsUtxos
  let reachedQuorum = Array.partition hasReachedQuorum finished

  logInfo' "Finished to process proposals"

-- markAsFailedToAchiveQuorum :: 
-- applyProposal ::

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
