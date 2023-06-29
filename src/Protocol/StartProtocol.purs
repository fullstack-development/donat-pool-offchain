module Protocol.StartProtocol where

import Contract.Prelude

import Config.Protocol (mapFromProtocolData, writeProtocolConfig)
import Contract.Address (getNetworkId, getWalletAddresses, getWalletAddressesWithNetworkTag, ownPaymentPubKeysHashes, addressToBech32, validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract, liftContractM, liftedM, liftedE)
import Contract.PlutusData (Redeemer(Redeemer), Datum(Datum), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array (head) as Array
import Data.BigInt (fromInt)
import Data.Map (toUnfoldable) as Map
import Ext.Contract.Value (mkCurrencySymbol)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolTokenName, protocolValidatorScript)
import Protocol.UserData (ProtocolConfigParams(..), ProtocolData, protocolToData)
import Shared.Config (mapFromProtocolConfigParams, writeDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.Utxo (filterNonCollateral)
import Effect.Aff (launchAff_)

initialProtocolConfigParams ∷ ProtocolConfigParams
initialProtocolConfigParams = ProtocolConfigParams
  { minAmountParam: fromInt 50000000
  , maxAmountParam: fromInt 1000000000
  , minDurationParam: fromInt 5
  , maxDurationParam: fromInt 86400
  , protocolFeeParam: fromInt 10
  }

runStartProtocol :: Effect Unit
runStartProtocol = launchAff_ $ runContract testnetKeyWalletConfig (contract initialProtocolConfigParams)

contract :: ProtocolConfigParams -> Contract ProtocolData
contract params@(ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  logInfo' "Running startDonatPool protocol contract"
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  ownBech32Address <- addressToBech32 ownAddress
  logInfo' $ "Own address is: " <> show ownBech32Address
  utxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show utxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (filterNonCollateral $ Map.toUnfoldable utxos))
  mp /\ cs <- mkCurrencySymbol (NFT.mintingPolicy oref)
  tn <- protocolTokenName
  let
    protocol = Protocol
      { protocolCurrency: cs
      , protocolTokenName: tn
      }
  let
    initialProtocolDatum = PProtocolDatum
      { minAmount: minAmountParam
      , maxAmount: maxAmountParam
      , minDuration: minDurationParam
      , maxDuration: maxDurationParam
      , protocolFee: protocolFeeParam
      , managerPkh: ownPkh
      , tokenOriginRef: oref
      }
    nftValue = Value.singleton cs tn one
    paymentToProtocol = Value.lovelaceValueOf (fromInt 2000000) <> nftValue
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolValidator <- protocolValidatorScript protocol

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput oref
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft tn)
          nftValue
        <> Constraints.mustPayToScriptAddress
          protocolValidatorHash
          (ScriptCredential protocolValidatorHash)
          (Datum $ toData initialProtocolDatum)
          Constraints.DatumInline
          paymentToProtocol

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos
        <> Lookups.validator protocolValidator

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <- liftedM "Failed to get own address with Network Tag" $ Array.head <$> getWalletAddressesWithNetworkTag
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId

  logInfo' $ "Current protocol: " <> show protocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  bech32Address <- addressToBech32 protocolAddress
  logInfo' $ "Current protocol address: " <> show bech32Address
  logInfo' "Transaction submitted successfully"
  protocolData <- protocolToData protocol

  let protocolConfig = mapFromProtocolData protocolData
  liftEffect $ writeProtocolConfig protocolConfig

  let donatPoolConfig = mapFromProtocolConfigParams params
  liftEffect $ writeDonatPoolConfig donatPoolConfig

  pure protocolData
