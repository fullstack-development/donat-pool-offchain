module Protocol.UpdateProtocol where

import Contract.Prelude

import Config.Protocol (mapToProtocolData, readProtocolConfig)
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
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Protocol.Datum (PProtocolDatum(..), _managerAddress, _tokenOriginRef)
import Protocol.Models (PProtocolConfig(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams, ProtocolData, dataToProtocol, getConfigFromProtocolDatum, mapToProtocolConfig)
import Shared.Config (mapToProtocolConfigParams, readDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (getPkhSkhFromAddress)
import Shared.Utxo (getNonCollateralUtxo)

runUpdateProtocol :: Effect Unit
runUpdateProtocol = do
  protocolConfig <- readProtocolConfig
  let protocolData = mapToProtocolData protocolConfig
  donatPoolConfig <- readDonatPoolConfig
  protocolConfigParams <- mapToProtocolConfigParams donatPoolConfig
  launchAff_ $ runContract testnetKeyWalletConfig (contract protocolData protocolConfigParams)

contract :: ProtocolData -> ProtocolConfigParams -> Contract ProtocolConfigParams
contract protocolData protocolConfigParams = do
  logInfo' "Running update protocol"
  protocol <- dataToProtocol protocolData
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol

  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  walletUtxo <- utxosAt ownAddress >>= getNonCollateralUtxo

  manager /\ _ <- getPkhSkhFromAddress $ view _managerAddress protocolInfo.pDatum
  when (manager /= ownPkh) $ liftEffect $ throw "Current user doesn't have permissions to update protocol"

  let protocolConfig = mapToProtocolConfig protocolConfigParams
  let newDatum = makeDatum protocolInfo.pDatum protocolConfig
  logInfo' $ "New datum: " <> show newDatum
  let newPDatum = Datum $ toData $ newDatum

  let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst protocolInfo.pUtxo)
        updateProtocolRedeemer
        protocolInfo.references.pRefScriptInput
        <> Constraints.mustPayToScriptAddress
          protocolInfo.pValidatorHash
          (ScriptCredential protocolInfo.pValidatorHash)
          newPDatum
          Constraints.DatumInline
          protocolInfo.pValue
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.pScriptRef)
        <> Constraints.mustBeSignedBy ownPkh
  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs protocolInfo.pUtxos
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
    , managerAddress: view _managerAddress currentDatum
    , tokenOriginRef: view _tokenOriginRef currentDatum
    }
