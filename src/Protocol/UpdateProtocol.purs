module Protocol.UpdateProtocol where

import Contract.Prelude
import Contract.Monad (ConfigParams, Contract, launchAff_, liftContractM, liftedE, liftedM, runContract)
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Protocol.Models (Protocol(..))
import Contract.Address (validatorHashBaseAddress, AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Lens (view)
import Protocol.Datum (PProtocolConfig, PProtocolDatum(..), _protocolConstants)
import Protocol.ProtocolScript (protocolValidatorScript, getProtocolValidatorHash, protocolTokenName)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Contract.Credential (Credential(ScriptCredential))
import Data.Array (head) as Array
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustSendChangeToAddress
  )
import Shared.Helpers (getNonCollateralUtxo, UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO, getUtxoByThreadToken)
import Protocol.UserData (ProtocolConfigParams(..), mapToProtocolConfig)
import Contract.Config (NetworkId(..), testnetNamiConfig)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Plutus.Types.CurrencySymbol as CurrencySymbol

runUpdateProtocol :: Effect Unit
runUpdateProtocol =
  let
    protocolParams =
      ProtocolConfigParams
        { minAmountParam: 70_000_000
        , maxAmountParam: 1_000_000_000
        , minDurationParam: 100
        , maxDurationParam: 1_000
        , protocolFeeParam: Tuple 10 100
        }
  in
    updateProtocol testnetNamiConfig protocolParams

-- TODO: pass protocol from frontend
getTestProtocol :: Contract () Protocol
getTestProtocol = do
  ownPkh <- ownPaymentPubKeyHash >>= liftContractM "no pkh found"
  cs <-
    liftContractM "Cannot make currency symbol" $
      CurrencySymbol.mkCurrencySymbol (hexToByteArrayUnsafe "a3963fd41c0da7cda3ffe5b832987d881f87d14f8e1b56da93fa74ce")
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
  protocolConfig <- runContract baseConfig (mapToProtocolConfig protocolConfigParams)
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
      Constraints.mustPayToScriptAddress protocolValidatorHash (ScriptCredential protocolValidatorHash) newDatum Constraints.DatumInline value
        <> Constraints.mustSpendScriptOutput (fst protocolUtxo) updateProtocolRedeemer

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
makeDatum currentDatum protocolConfig =
  let
    protocolConstants = view _protocolConstants currentDatum
  in
    PProtocolDatum { protocolConstants, protocolConfig }

getProtocolUtxo :: Protocol -> UtxoMap -> Contract () UtxoTuple
getProtocolUtxo protocol utxos =
  let
    p = unwrap protocol
  in
    getUtxoByThreadToken (Tuple (_.protocolCurrency p) (_.protocolTokenName p)) utxos
