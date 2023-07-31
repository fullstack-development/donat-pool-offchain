module Shared.Tx where

import Contract.Prelude

import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class ToData, Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Shared.OwnCredentials (OwnCredentials(..))

completeTx
  :: Lookups.ScriptLookups Void
  -> Constraints.TxConstraints Void Void
  -> OwnCredentials
  -> Contract Unit
completeTx lookups constraints (OwnCredentials ownCreds) = do
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress ownCreds.ownAddressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId

toDatum :: forall a. ToData a => a -> Datum
toDatum d = Datum $ toData d

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer r = Redeemer $ toData r
