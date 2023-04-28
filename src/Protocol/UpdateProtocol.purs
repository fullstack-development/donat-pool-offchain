module Protocol.UpdateProtocol where

import Contract.Prelude

import Contract.Address (getWalletAddressesWithNetworkTag, getWalletAddresses, ownPaymentPubKeysHashes)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
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
import Effect.Aff (runAff_)
import Effect.Exception (Error, message, throw)
import Protocol.Datum (PProtocolDatum(..), _managerPkh, _tokenOriginRef)
import Protocol.Models (PProtocolConfig(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams, ProtocolData, dataToProtocol, getConfigFromProtocolDatum, mapToProtocolConfig)
import Shared.Helpers (getNonCollateralUtxo)
import Shared.TestnetConfig (mkTestnetNamiConfig)

runUpdateProtocol :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> ProtocolConfigParams -> Effect Unit
runUpdateProtocol onComplete onError protocolData params = do
  testnetNamiConfig <- mkTestnetNamiConfig
  let protocolConfig = mapToProtocolConfig params
  runAff_ handler $ runContract testnetNamiConfig (contract protocolData protocolConfig)
  where
  handler :: Either Error ProtocolConfigParams -> Effect Unit
  handler (Right protocolConfigParams) = onComplete protocolConfigParams
  handler (Left error) = onError $ message error

contract :: ProtocolData -> PProtocolConfig -> Contract ProtocolConfigParams
contract protocolData protocolConfig = do
  logInfo' "Running update protocol"
  protocol <- dataToProtocol protocolData
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  walletUtxo <- utxosAt ownAddress >>= getNonCollateralUtxo

  let manager = view _managerPkh protocolInfo.pDatum
  when (manager /= ownPkh) $ liftEffect $ throw "Current user doesn't have permissions to update protocol"

  let newDatum = makeDatum protocolInfo.pDatum protocolConfig
  logInfo' $ "New datum: " <> show newDatum
  let newPDatum = Datum $ toData $ newDatum

  let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst protocolInfo.pUtxo) updateProtocolRedeemer
        <> Constraints.mustPayToScriptAddress
          protocolInfo.pValidatorHash
          (ScriptCredential protocolInfo.pValidatorHash)
          newPDatum
          Constraints.DatumInline
          protocolInfo.pValue
        <> Constraints.mustBeSignedBy ownPkh
  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.validator protocolInfo.pValidator
        <> Lookups.unspentOutputs protocolInfo.pUtxos
        <> Lookups.unspentOutputs walletUtxo

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <- liftedM "Failed to get own address with Network Tag" $ Array.head <$> getWalletAddressesWithNetworkTag
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  pure $ getConfigFromProtocolDatum newDatum

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
