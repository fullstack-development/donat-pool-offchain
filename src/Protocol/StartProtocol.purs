module Protocol.StartProtocol where

import Contract.Prelude

import Contract.Address 
  ( getWalletAddresses
  , getWalletAddressesWithNetworkTag
  , ownPaymentPubKeysHashes
  , addressToBech32
  , validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Config (testnetNamiConfig, NetworkId(TestnetId))
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
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript, protocolTokenName)
import Protocol.UserData (ProtocolConfigParams(..))
import Shared.Helpers as Helpers

runStartProtocol :: (Protocol -> Effect Unit) -> (String -> Effect Unit) -> ProtocolConfigParams -> Effect Unit
runStartProtocol onComplete onError params = runAff_ handler $
  runContract testnetNamiConfig (contract params)
  where
  handler :: Either Error Protocol -> Effect Unit
  handler (Right protocol) = onComplete protocol
  handler (Left error) = onError $ message error

contract :: ProtocolConfigParams -> Contract () Protocol
contract (ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  logInfo' "Running startDonatPool protocol contract"
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  logInfo' $ "Own address is: " <> show ownAddress
  utxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show utxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Helpers.filterNonCollateral $ Map.toUnfoldable utxos))
  mp /\ cs <- Helpers.mkCurrencySymbol (NFT.mintingPolicy oref)
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
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  bech32Address <- addressToBech32 protocolAddress
  logInfo' $ "Current protocol address: " <> show bech32Address
  logInfo' "Transaction submitted successfully"
  pure protocol
