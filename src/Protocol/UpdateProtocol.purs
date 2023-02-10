module Protocol.UpdateProtocol where

import Contract.Prelude

import Contract.Address (validatorHashBaseAddress, AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Config (NetworkId(..), testnetNamiConfig)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Array (head) as Array
import Data.Lens (view)
import Info.Protocol (getProtocolUtxo)
import Protocol.Datum (PProtocolDatum(..), _managerPkh, _tokenOriginRef)
import Protocol.Models (PProtocolConfig(..), Protocol)
import Protocol.ProtocolScript (protocolValidatorScript, getProtocolValidatorHash)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams, mapToProtocolConfig, mapFromProtocolDatum)
import Shared.Helpers (getNonCollateralUtxo, extractDatumFromUTxO, extractValueFromUTxO)
import Effect.Aff (runAff_)
import Effect.Exception (Error, message, throw)

runUpdateProtocol :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> ProtocolConfigParams -> Effect Unit
runUpdateProtocol onComplete onError protocol params = runAff_ handler $ do
  let protocolConfig = mapToProtocolConfig params
  runContract testnetNamiConfig (contract protocol protocolConfig)
  where
  handler :: Either Error ProtocolConfigParams -> Effect Unit
  handler (Right protocolConfigParams) = onComplete protocolConfigParams
  handler (Left error) = onError $ message error

contract :: Protocol -> PProtocolConfig -> Contract () ProtocolConfigParams
contract protocol protocolConfig = do
  logInfo' "Running update protocol"
  protocolValidator <- protocolValidatorScript protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos

  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  let manager = _.managerPkh $ unwrap protocol
  when (manager /= ownPkh) $ liftEffect $ throw "current user doesn't have permissions to close protocol"

  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  walletUtxo <- utxosAt ownAddress >>= getNonCollateralUtxo

  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  let value = extractValueFromUTxO protocolUtxo
  logInfo' $ "Current value: " <> show value

  let newDatum = makeDatum currentDatum protocolConfig
  logInfo' $ "New datum: " <> show newDatum
  let newPDatum = Datum $ toData $ makeDatum currentDatum protocolConfig

  let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst protocolUtxo) updateProtocolRedeemer
        <> Constraints.mustPayToScriptAddress protocolValidatorHash (ScriptCredential protocolValidatorHash) newPDatum Constraints.DatumInline value
        <> Constraints.mustBeSignedBy ownPkh
  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.validator protocolValidator
        <> Lookups.unspentOutputs utxos
        <> Lookups.unspentOutputs walletUtxo

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    addressWithNetworkTag =
      AddressWithNetworkTag
        { address: ownAddress
        , networkId: TestnetId
        }

    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  pure $ mapFromProtocolDatum newDatum

makeDatum ∷ PProtocolDatum -> PProtocolConfig → PProtocolDatum
makeDatum currentDatum (PProtocolConfig { minAmount, maxAmount, minDuration, maxDuration, protocolFee }) =
  PProtocolDatum
    { minAmount: minAmount
    , maxAmount: maxAmount
    , minDuration: minDuration
    , maxDuration: maxDuration
    , protocolFee: protocolFee
    , managerPkh: view _managerPkh currentDatum
    , tokenOriginRef: view _tokenOriginRef currentDatum
    }
