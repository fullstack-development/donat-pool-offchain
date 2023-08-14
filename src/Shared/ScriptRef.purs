module Shared.ScriptRef where

import Contract.Prelude

import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), ValidatorHash)
import Contract.Transaction (ScriptRef(..), awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Effect.Exception (throw)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.FundraisingScriptInfo (makeFundraising)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (sevenMinAdaValue)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)

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
