module Protocol.UpdateProtocol where

import Contract.Prelude

import Contract.Address (validatorHashBaseAddress, AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Config (NetworkId(..), testnetNamiConfig)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (ConfigParams, Contract, launchAff_, liftContractM, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.CurrencySymbol as CurrencySymbol
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Array (head) as Array
import Data.Lens (view)
import Effect.Exception (throw)
import Info.Protocol (getProtocolUtxo)
import Protocol.Datum (PProtocolDatum(..), _managerPkh, _tokenOriginRef)
import Protocol.Models (Protocol(..), PProtocolConfig(..))
import Protocol.ProtocolScript (protocolValidatorScript, getProtocolValidatorHash, protocolTokenName)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams(..), mapToProtocolConfig)
import Shared.Helpers (getNonCollateralUtxo, extractDatumFromUTxO, extractValueFromUTxO)
import Data.BigInt (fromInt)

runUpdateProtocol :: Effect Unit
runUpdateProtocol =
  let
    protocolParams =
      ProtocolConfigParams
        { minAmountParam: fromInt 70_000_000
        , maxAmountParam: fromInt 1_000_000_000
        , minDurationParam: fromInt 100
        , maxDurationParam: fromInt 1_000
        , protocolFeeParam: fromInt 10
        }
  in
    updateProtocol testnetNamiConfig protocolParams

-- TODO: pass protocol from frontend
getTestProtocol :: Contract () Protocol
getTestProtocol = do
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  cs <-
    liftContractM "Cannot make currency symbol" $
      CurrencySymbol.mkCurrencySymbol (hexToByteArrayUnsafe "4fcf285d0d75ec4b5077e31e2279b3cc97b996082f185bd7a504a7c0")
  tn <- protocolTokenName
  let
    protocol =
      Protocol
        { managerPkh: ownPkh
        , protocolCurrency: cs
        , protocolTokenName: tn
        }
  pure protocol

updateProtocol :: ConfigParams () -> ProtocolConfigParams -> Effect Unit
updateProtocol baseConfig protocolConfigParams = launchAff_ $ do
  protocol <- runContract baseConfig getTestProtocol
  let protocolConfig = mapToProtocolConfig protocolConfigParams
  runContract baseConfig (contract protocol protocolConfig)

contract :: Protocol -> PProtocolConfig -> Contract () Unit
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

  let newDatum = Datum $ toData $ makeDatum currentDatum protocolConfig
  logInfo' $ "New datum: " <> show newDatum

  let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst protocolUtxo) updateProtocolRedeemer
        <> Constraints.mustPayToScriptAddress protocolValidatorHash (ScriptCredential protocolValidatorHash) newDatum Constraints.DatumInline value
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
