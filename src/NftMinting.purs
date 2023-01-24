module NftMinting where

import Contract.Prelude

import Contract.Address (getWalletAddresses)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftContractE, liftedM)
import Contract.PlutusData
  ( PlutusData
  , Redeemer(Redeemer)
  , genericToData
  , class ToData
  , class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  , toData
  , S
  , Z
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
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
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, singleton, filter) as Array
import Data.BigInt (fromInt)
import Data.Lens.Getter ((^.))
import Data.Map (toUnfoldable) as Map
--import Data.Tuple (Tuble) as Tuple
import Effect.Exception (error)

data PNftRedeemer = PMintNft Value.TokenName | PBurnNft Value.TokenName

derive instance Generic PNftRedeemer _

instance
  HasPlutusSchema
    PNftRedeemer
    ( "PMintNft" := PNil @@ Z
        :+ "PBurnNft"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData PNftRedeemer where
  toData = genericToData

main :: Effect Unit
main = mintNft testnetNamiConfig

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

-- NOTE: Nami wallet doesn't allow to spend the collateral UTxO - a special UTxO with 5 Ada balance.
-- As this UTxO doesn't have any special flags, we get the first UTxO with balance non equal to 5 Ada
adaCollateralValue :: Value.Value
adaCollateralValue = Value.singleton Value.adaSymbol Value.adaToken (fromInt 5000000)

checkNonCollateral :: Tuple TransactionInput TransactionOutputWithRefScript -> Boolean
checkNonCollateral (Tuple _ txOutWithRef) =
  let
    utxoValue = (txOutWithRef ^. _output) ^. _amount
  in
    utxoValue /= adaCollateralValue

filterNonCollateral
  :: Array (Tuple TransactionInput TransactionOutputWithRefScript)
  -> Array (Tuple TransactionInput TransactionOutputWithRefScript)
filterNonCollateral = Array.filter checkNonCollateral

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

mkCurrencySymbol
  :: forall (r :: Row Type)
   . Contract r MintingPolicy
  -> Contract r (MintingPolicy /\ Value.CurrencySymbol)
mkCurrencySymbol policy = do
  mp <- policy
  cs <- liftContractM "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

mkTokenName :: forall (r :: Row Type). String -> Contract r Value.TokenName
mkTokenName =
  liftContractM "Cannot make token name"
    <<< (Value.mkTokenName <=< byteArrayFromAscii)
