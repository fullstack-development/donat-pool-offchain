module Shared.ScriptRef where

import Contract.Prelude

import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), PlutusScript)
import Contract.Transaction (ScriptRef(..), awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Types.Scripts (ValidatorHash)
import Data.Array as Array
import Data.Map as Map
import Effect.Exception (throw)
import Ext.Contract.Value (mkCurrencySymbol)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.FundraisingScriptInfo (makeFundraising)
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceValidatorScript)
import MintingPolicy.VerTokenMinting as VerToken
import Proposal.Model (mkProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalValidatorScript)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (sevenMinAdaValue)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.Utxo (UtxoTuple)

createRefScriptUtxo ∷ String -> ScriptRef -> ValidatorHash → Contract Unit
createRefScriptUtxo _ (NativeScriptRef _) _ = liftEffect $ throw "Unexpected scriptRef type: waiting for PlutusScriptRef"
createRefScriptUtxo scriptName scriptRef@(PlutusScriptRef _) validatorHash = do
  logInfo' $ "Start to create " <> scriptName <> " reference script"
  (OwnCredentials creds) <- getOwnCreds

  let
    constraints :: Constraints.TxConstraints Unit Unit
    constraints = Constraints.mustPayToScriptAddressWithScriptRef
      validatorHash
      (ScriptCredential validatorHash)
      unitDatum
      Constraints.DatumWitness
      scriptRef
      sevenMinAdaValue

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints

  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress creds.ownAddressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ scriptName <> " UTxO with reference script created"

mkProtocolRefScript :: ProtocolData -> Contract Unit
mkProtocolRefScript protocolData = do
  protocol <- dataToProtocol protocolData
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolValidator <- protocolValidatorScript protocol
  let scriptRef = PlutusScriptRef (unwrap protocolValidator)
  createRefScriptUtxo "Protocol" scriptRef protocolValidatorHash

mkFundraisingRefScript :: ProtocolData -> Contract Unit
mkFundraisingRefScript protocolData = do
  fundraising <- makeFundraising protocolData
  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising
  let scriptRef = PlutusScriptRef (unwrap frValidator)
  createRefScriptUtxo "Fundraising" scriptRef frValidatorHash

mkProposalRefScript :: Protocol -> Contract Unit
mkProposalRefScript protocol = do
  _ /\ proposalVerTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  let proposal = mkProposal protocol proposalVerTokenCs
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalValidator <- proposalValidatorScript proposal
  let scriptRef = PlutusScriptRef (unwrap proposalValidator)
  createRefScriptUtxo "Proposal" scriptRef proposalValidatorHash

mkGovernanceRefScript :: Protocol -> Contract Unit
mkGovernanceRefScript protocol = do
  governanceValidatorHash <- getGovernanceValidatorHash protocol
  governanceValidator <- governanceValidatorScript protocol
  let scriptRef = PlutusScriptRef (unwrap governanceValidator)
  createRefScriptUtxo "Governance" scriptRef governanceValidatorHash

createPolicyRefUtxo :: String -> MintingPolicy → ValidatorHash → Contract Unit
createPolicyRefUtxo _ (NativeMintingPolicy _) _ = liftEffect $ throw "Unexpected minting policy type"
createPolicyRefUtxo mpName (PlutusMintingPolicy policy) validatorHash = do
  logInfo' $ "Creating UTxO with " <> mpName <> " minting policy reference"
  let scriptRef = PlutusScriptRef policy
  createRefScriptUtxo "VerTokenPolicy" scriptRef validatorHash
  logInfo' $ "UTxO with " <> mpName <> " minting policy reference created"

mkVerTokenPolicyRef :: ProtocolData -> Contract Unit
mkVerTokenPolicyRef protocolData = do
  protocol <- dataToProtocol protocolData
  protocolValidatorHash <- getProtocolValidatorHash protocol
  policy <- VerToken.mintingPolicy protocol
  createPolicyRefUtxo "VerToken" policy protocolValidatorHash

hasExpectedRefScript :: PlutusScript -> UtxoTuple -> Boolean
hasExpectedRefScript plutusScript (_ /\ txOutput) =
  (unwrap txOutput).scriptRef == Just (PlutusScriptRef plutusScript)

findUtxoWithRefScript :: PlutusScript -> UtxoMap -> Maybe UtxoTuple
findUtxoWithRefScript plutusScript utxoMap =
  let
    (utxoArray :: Array UtxoTuple) = Map.toUnfoldable utxoMap
  in
    Array.find (hasExpectedRefScript plutusScript) utxoArray

getUtxoWithRefScript :: PlutusScript -> UtxoMap -> Contract UtxoTuple
getUtxoWithRefScript plutusScript utxoMap =
  liftContractM "UTxO with expected reference script not found" $ findUtxoWithRefScript plutusScript utxoMap
