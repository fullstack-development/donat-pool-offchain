module MintingPolicy.NftMinting where

import Contract.Prelude

import Contract.Address (getWalletAddresses)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftContractE, liftedM)
import Contract.PlutusData (PlutusData, Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  )
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  , TransactionOutputWithRefScript
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, singleton) as Array
import Data.Map (toUnfoldable) as Map
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Effect.Exception (error)
import Shared.Helpers (mkTokenName, mkCurrencySymbol, filterNonCollateral)

runMintNft :: Effect Unit
runMintNft = mintNft testnetNamiConfig

mintNft :: ConfigParams () -> Effect Unit
mintNft cfg = launchAff_ do
  runContract cfg contract

contract :: Contract () Unit
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
  tn <- mkTokenName "MyLovelyNFT"
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
  logInfo' $ "Tx submitted successfully!"

foreign import nftPolicy :: String

mintingPolicy :: TransactionInput -> Contract () MintingPolicy
mintingPolicy =
  map PlutusMintingPolicy <<< mintNftPolicyScript

mintNftPolicyScript :: TransactionInput -> Contract () PlutusScript
mintNftPolicyScript txInput = do
  script <- liftMaybe (error "Error decoding nftPolicy") do
    envelope <- decodeTextEnvelope nftPolicy
    plutusScriptV1FromEnvelope envelope
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
