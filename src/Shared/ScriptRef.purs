module Shared.ScriptRef where

import Contract.Prelude
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceValidatorScript)
import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (ScriptRef(..), awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Data.Array (head) as Array
import Data.BigInt (fromInt)
import Ext.Seriaization.Token (deserializeCurrency)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.FundraisingScriptInfo (makeFundraising)
import Governance.Config (readGovernanceConfig)
import Proposal.Model (PProposal(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (minAda)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.Scripts.Proposal.Script (getProposalValidatorHash, proposalValidatorScript)

createRefScriptUtxo ∷ String -> ValidatorHash → Validator → Contract Unit
createRefScriptUtxo scriptName validatorHash validator = do
  logInfo' $ "Start to create " <> scriptName <> " reference script"
  (OwnCredentials creds) <- getOwnCreds
  let scriptRef = PlutusScriptRef (unwrap validator)

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
    lookups = Lookups.unspentOutputs creds.ownUtxos

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <-
    liftedM "Failed to get own address with Network Tag"
      $ Array.head
      <$> getWalletAddressesWithNetworkTag

  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ scriptName <> " UTxO with reference script created"
  where
  sevenMinAdaValue = Value.lovelaceValueOf (minAda * (fromInt 7))

mkProtocolRefScript :: ProtocolData -> Contract Unit
mkProtocolRefScript protocolData = do
  protocol <- dataToProtocol protocolData
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolValidator <- protocolValidatorScript protocol
  createRefScriptUtxo "Protocol" protocolValidatorHash protocolValidator

mkFundraisingRefScript :: ProtocolData -> Contract Unit
mkFundraisingRefScript protocolData = do
  fundraising <- makeFundraising protocolData
  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising
  createRefScriptUtxo "Fundraising" frValidatorHash frValidator

mkProposalRefScript :: Protocol -> Contract Unit
mkProposalRefScript (Protocol protocol) = do
  governanceConfig <- liftEffect readGovernanceConfig
  governanceCurrency <- deserializeCurrency governanceConfig.governanceCurrency
  let proposal = PProposal {
    protocolCurrency: protocol.protocolCurrency
    , govCurrency: governanceCurrency
  }
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalValidator <- proposalValidatorScript proposal
  createRefScriptUtxo "Proposal" proposalValidatorHash proposalValidator

mkGovernanceRefScript :: Protocol -> Contract Unit
mkGovernanceRefScript protocol = do
  governanceValidatorHash <- getGovernanceValidatorHash protocol
  governanceValidator <- governanceValidatorScript protocol
  createRefScriptUtxo "Governance" governanceValidatorHash governanceValidator