module MintingPolicy.NftMinting where

import Contract.Prelude

import Contract.Address (getWalletAddresses)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftContractE, liftedM)
import Contract.PlutusData (PlutusData, Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction
  ( TransactionInput(..)
  , awaitTxConfirmed
  , submitTxFromConstraints
  , TransactionOutputWithRefScript
  , TransactionHash(..)
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, singleton) as Array
import Data.Map (toUnfoldable) as Map
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Effect.Exception (error)
import Shared.Utxo (filterNonCollateral, filterByToken)
import Ext.Contract.Value (runMkTokenName, mkCurrencySymbol)
import Data.BigInt (fromInt)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Data.UInt as UInt

runMintNft :: Effect Unit
runMintNft = mintNft testnetNamiConfig

runBurnNft :: Effect Unit
runBurnNft = burnNft testnetNamiConfig

mintNft :: ContractParams -> Effect Unit
mintNft cfg = launchAff_ do
  runContract cfg contract

burnNft :: ContractParams -> Effect Unit
burnNft cfg = launchAff_ do
  -- NOTE: Provide with correct Transaction input
  let
    oref =
      TransactionInput
        { index: UInt.fromInt 1
        , transactionId: TransactionHash (hexToByteArrayUnsafe "9d94f35a95e2d14e670f0ba2f6b7b28904b9ac01b5666c90ddd96627cbddfdcc")
        }
  runContract cfg (burnNftContract oref)

contract :: Contract Unit
contract = do
  logInfo' "Running mint NFT contract"
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$>
    getWalletAddresses
  logInfo' $ "Own address is: " <> show ownAddress
  utxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show utxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (filterNonCollateral (Map.toUnfoldable utxos :: Array (Tuple TransactionInput TransactionOutputWithRefScript))))
  mp /\ cs <- mkCurrencySymbol (mintingPolicy oref)
  tn <- runMkTokenName "MyLovelyNFT"
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput oref
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft tn)
          (Value.singleton cs tn one)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId

  logInfo' $ "Token origin: " <> show oref
  logInfo' "Tx submitted successfully!"

burnNftContract :: TransactionInput -> Contract Unit
burnNftContract txInput = do
  logInfo' "Running burn NFT contract"
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$>
    getWalletAddresses
  logInfo' $ "Own address is: " <> show ownAddress
  mp /\ cs <- mkCurrencySymbol (mintingPolicy txInput)
  tn <- runMkTokenName "MyLovelyNFT"
  let nft = Tuple cs tn
  utxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show utxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (filterByToken nft (Map.toUnfoldable utxos)))
  logInfo' $ "Desired ORef: " <> show oref

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer $ toData $ PBurnNft tn)
        (Value.singleton cs tn (fromInt (-1)))

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' $ "Tx submitted successfully!"

foreign import nftPolicy :: String

mintingPolicy :: TransactionInput -> Contract MintingPolicy
mintingPolicy =
  map PlutusMintingPolicy <<< mintNftPolicyScript

mintNftPolicyScript :: TransactionInput -> Contract PlutusScript
mintNftPolicyScript txInput = do
  script <- liftMaybe (error "Error decoding nftPolicy") do
    envelope <- decodeTextEnvelope nftPolicy
    plutusScriptV2FromEnvelope envelope
  liftContractE $ mkMintNftPolicy script txInput

mkMintNftPolicy
  :: PlutusScript
  -> TransactionInput
  -> Either ApplyArgsError PlutusScript
mkMintNftPolicy unappliedMintingPolicy oref =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData oref)
  in
    applyArgs unappliedMintingPolicy mintingPolicyArgs
