module Protocol.StartProtocol where

import Contract.Prelude

import Contract.Address (getWalletAddresses, ownPaymentPubKeysHashes, AddressWithNetworkTag(..), addressToBech32, validatorHashBaseAddress)
import Contract.Config (ConfigParams, testnetNamiConfig, NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftedM, liftedE)
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , Datum(Datum)
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array (head) as Array
import Data.BigInt (fromInt)
import Data.Map (toUnfoldable) as Map
import Protocol.UserData (ProtocolConfigParams(..))
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.NftMinting as NFT
import Shared.Helpers as Helpers
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript, protocolTokenName)
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustSendChangeToAddress
  )
import Protocol.Datum
  ( PProtocolDatum(..)
  )
import Contract.Credential (Credential(ScriptCredential))

runStartProtocolTest :: Effect Unit
runStartProtocolTest =
  let
    protocolParams =
      ProtocolConfigParams
        { minAmountParam: fromInt 50_000_000
        , maxAmountParam: fromInt 1_000_000_000
        , minDurationParam: fromInt 100
        , maxDurationParam: fromInt 1_000
        , protocolFeeParam: fromInt 10
        }
  in
    startProtocol testnetNamiConfig protocolParams

startProtocol :: ConfigParams () -> ProtocolConfigParams -> Effect Unit
startProtocol baseConfig protocolConfig = launchAff_ do
  runContract baseConfig (contract protocolConfig)

contract :: ProtocolConfigParams -> Contract () Unit
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
      { managerPkh: ownPkh
      , protocolCurrency: cs
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

  logInfo' $ "Current protocol: " <> show protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  bech32Address <- addressToBech32 protocolAddress
  logInfo' $ "Current protocol address: " <> show bech32Address
  logInfo' "Transaction submitted successfully"

