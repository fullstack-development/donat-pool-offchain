module Shared.ScriptRef where

import Contract.Prelude

import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (ScriptRef(..), awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Data.Array (head) as Array
import Shared.MinAda (minAdaValue)
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
            minAdaValue

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.unspentOutputs creds.ownUtxo

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <- 
    liftedM "Failed to get own address with Network Tag" 
        $ Array.head <$> getWalletAddressesWithNetworkTag

  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' $ scriptName <> " reference script created"
