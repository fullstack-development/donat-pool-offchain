module Shared.Tx where

import Contract.Prelude

import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Log (logError')
import Contract.Monad (Contract)
import Contract.PlutusData (class ToData, Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submitE)
import Contract.TxConstraints as Constraints
import Data.Either (either)
import Effect.Exception (throw)
import Shared.OwnCredentials (OwnCredentials(..))

completeTx
  :: Lookups.ScriptLookups Void
  -> Constraints.TxConstraints Void Void
  -> OwnCredentials
  -> Contract Unit
completeTx lookups constraints (OwnCredentials ownCreds) = do
  unbalancedTx <- Lookups.mkUnbalancedTx lookups constraints >>= either (handleError mkTxMsg) pure
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress ownCreds.ownAddressWithNetworkTag
  balancedTx <- balanceTxWithConstraints unbalancedTx balanceTxConstraints >>= either (handleError balanceTxMsg) pure
  balancedSignedTx <- signTransaction balancedTx
  submitE balancedSignedTx >>= either (handleError submitTxMsg) awaitTxConfirmed
  where
  mkTxMsg = "An error occured while constructing the transaction"
  balanceTxMsg = "An error occured while balancing the transaction"
  submitTxMsg = "An error occured while submitting the transaction"

  handleError :: forall a err. Show err => String -> err -> Contract a
  handleError errMsg err = do
    logError' $ show err
    liftEffect $ throw errMsg

toDatum :: forall a. ToData a => a -> Datum
toDatum d = Datum $ toData d

toRedeemer :: forall a. ToData a => a -> Redeemer
toRedeemer r = Redeemer $ toData r
