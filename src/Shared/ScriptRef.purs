module Shared.ScriptRef where

import Contract.Prelude

import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), Validator, ValidatorHash)
import Contract.Transaction (ScriptRef(..), awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.FundraisingScriptInfo (makeFundraising)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (minAda)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)

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

createPolicyRefUtxo :: String -> MintingPolicy → Contract Unit
createPolicyRefUtxo _ (NativeMintingPolicy _) = liftEffect $ throw "Unexpected minting policy type"
createPolicyRefUtxo mpName (PlutusMintingPolicy policy) = do
  logInfo' $ "Creating UTxO with " <> mpName <> " minting policy name"
  (OwnCredentials creds) <- getOwnCreds
  let
    scriptRef = PlutusScriptRef policy
    sevenMinAdaValue = Value.lovelaceValueOf (minAda * (fromInt 7))

    constraints :: Constraints.TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToPubKeyAddressWithScriptRef creds.ownPkh creds.ownSkh scriptRef sevenMinAdaValue

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
  logInfo' $ "UTxO with " <> mpName <> " minting policy reference created"

mkVerTokenPolicyRef :: ProtocolData -> Contract Unit
mkVerTokenPolicyRef protocolData = do
  protocol <- dataToProtocol protocolData
  policy <- VerToken.mintingPolicy protocol
  createPolicyRefUtxo "VerToken" policy
