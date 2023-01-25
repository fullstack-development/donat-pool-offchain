module Protocol.CloseProtocol where

import Contract.Prelude

import Contract.Address
  ( getWalletAddresses
  , ownPaymentPubKeyHash
  , ownPaymentPubKeysHashes
  , AddressWithNetworkTag(..)
  , validatorHashBaseAddress
  , addressToBech32
  )
import Contract.Config (ConfigParams, testnetNamiConfig, NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftedM, liftedE)
import Contract.PlutusData
  ( Redeemer(Redeemer)
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
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.NftMinting as NFT
import Shared.Helpers as Helpers
import Protocol.Models (PProtocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript, protocolTokenName)
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustSendChangeToAddress
  )
import Protocol.Datum
  ( PProtocolConstants(..)
  , PProtocolDatum(..)
  )
import Effect.Exception (throw)
import Protocol.Redeemer (PProtocolRedeemer(PCloseProtocol))
--import Ctl.Internal.Types.RawBytes (hexToRawBytesUnsafe)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Plutus.Types.CurrencySymbol as CurrencySymbol

runCloseProtocolTest :: Effect Unit
runCloseProtocolTest = do
  closeProtocol testnetNamiConfig

closeProtocol :: ConfigParams () -> Effect Unit
closeProtocol baseConfig = launchAff_ do
  protocol <- runContract baseConfig getTestProtocol
  runContract baseConfig (contract protocol)

-- NOTE: provide protocol fields with correct values
getTestProtocol :: Contract () PProtocol
getTestProtocol = do
  ownPkh <- ownPaymentPubKeyHash >>= liftContractM "no pkh found"
  cs <-
    liftContractM "Cannot make currency symbol" $
      CurrencySymbol.mkCurrencySymbol (hexToByteArrayUnsafe "47c97d9f80fc21a3f12a0131c3a80ad181d5d940cf4a22250bd68d26")
  tn <- protocolTokenName
  let
    protocol =
      PProtocol
        { managerPkh: ownPkh
        , protocolCurrency: cs
        , protocolTokenName: tn
        }
  pure protocol

contract :: PProtocol -> Contract () Unit
contract protocol@(PProtocol { managerPkh, protocolCurrency, protocolTokenName }) = do
  logInfo' "Running closeProtocol"
  logInfo' $ "Current protocol: " <> show protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  bech32Address <- addressToBech32 protocolAddress
  logInfo' $ "Current protocol address: " <> show bech32Address
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  when (managerPkh /= ownPkh) $ liftEffect $ throw "current user doesn't have permissions to close protocol"

  protocolUTxOs <- utxosAt protocolAddress
  logInfo' $ "Protocol UTxOs: " <> show protocolUTxOs
  let protocolNft = Tuple protocolCurrency protocolTokenName
  desiredTxOut <-
    liftContractM "Protocol UTxO with current TrheadToken not found"
      (Array.head (Helpers.filterByToken protocolNft $ Map.toUnfoldable protocolUTxOs))

  PProtocolDatum protocolDatum <- liftContractM "Impossible to get Protocol Datum" $ Helpers.extractDatumFromUTxO desiredTxOut
  let
    PProtocolConstants constants = protocolDatum.protocolConstants
    nftOref = constants.tokenOrigin
  mp <- NFT.mintingPolicy nftOref
  protocolValidator <- protocolValidatorScript protocol
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  let
    protocolRedeemer = Redeemer $ toData PCloseProtocol
    nftToBurnValue = Value.singleton protocolCurrency protocolTokenName (fromInt (-1))

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput
        (fst desiredTxOut)
        protocolRedeemer
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft protocolTokenName)
          nftToBurnValue

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs protocolUTxOs
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

  logInfo' "closeProtocol finished successfully"

