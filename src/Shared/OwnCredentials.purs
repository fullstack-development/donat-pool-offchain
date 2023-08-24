module Shared.OwnCredentials where

import Contract.Prelude

import Contract.Address (Address, AddressWithNetworkTag, PaymentPubKeyHash(..), PubKeyHash, StakePubKeyHash(..), addressWithNetworkTagToBech32, getWalletAddressesWithNetworkTag, ownPaymentPubKeysHashes, ownStakePubKeysHashes, toPubKeyHash, toStakingCredential)
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt (BigInt, toString)
import Data.Map as Map
import Effect.Exception (throw)
import Ext.Contract.Value (currencySymbolToString, tokenNameToString)
import Info.UserData (UserInfo(..))
import Shared.Utxo (UtxoTuple, extractValueFromUTxO, getNonCollateralUtxo)

newtype OwnCredentials = OwnCredentials
  { ownPkh :: PaymentPubKeyHash
  , ownSkh :: StakePubKeyHash
  , ownAddressWithNetworkTag :: AddressWithNetworkTag
  , ownUtxos :: (Map.Map TransactionInput TransactionOutputWithRefScript)
  , ownValue :: Value.Value
  , nonCollateralORef :: TransactionInput
  }

getOwnCreds :: Contract OwnCredentials
getOwnCreds = do
  (ownPkh /\ ownAddressWithNetworkTag) <- getOwnPkhAndAddress
  mbOwnSkh <- join <<< Array.head <$> ownStakePubKeysHashes
  ownSkh <- liftContractM "Failed to get own SKH" mbOwnSkh
  allUtxos <- utxosAt ownAddressWithNetworkTag
  nonCollateralUtxos <- getNonCollateralUtxo allUtxos

  let (utxosArray :: Array UtxoTuple) = Map.toUnfoldable allUtxos
  let ownValue = foldMap extractValueFromUTxO utxosArray

  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable nonCollateralUtxos :: Array UtxoTuple))
  pure $ OwnCredentials
    { ownPkh: ownPkh
    , ownSkh: ownSkh
    , ownAddressWithNetworkTag: ownAddressWithNetworkTag
    , ownUtxos: nonCollateralUtxos
    , ownValue: ownValue
    , nonCollateralORef: oref
    }

getOwnUserInfo :: PaymentPubKeyHash -> Contract UserInfo
getOwnUserInfo managerPkh = do
  (ownPkh /\ ownAddressWithNetworkTag) <- getOwnPkhAndAddress
  pure $ UserInfo
    { address: addressWithNetworkTagToBech32 ownAddressWithNetworkTag
    , isManager: ownPkh == managerPkh
    }

getOwnPkhAndAddress ∷ Contract (Tuple PaymentPubKeyHash AddressWithNetworkTag)
getOwnPkhAndAddress = do
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddressWithNetworkTag <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddressesWithNetworkTag
  pure $ (ownPkh /\ ownAddressWithNetworkTag)

getPkhSkhFromAddress :: Address -> Contract (PaymentPubKeyHash /\ StakePubKeyHash)
getPkhSkhFromAddress address = do
  pkh <- liftContractM "Impossible to extract payment pkh from script address" $ toPubKeyHash address
  stakingCreds <- liftContractM "Staking creds missed from provided address" $ toStakingCredential address
  skh <- case stakingCreds of
    StakingHash creds -> liftContractM "Impossible to extract staking pkh" $ pkhFromCreds creds
    _ -> liftEffect $ throw "Unexpected staking credentials"
  pure (PaymentPubKeyHash pkh /\ StakePubKeyHash skh)
  where
  pkhFromCreds :: Credential -> Maybe PubKeyHash
  pkhFromCreds creds = case creds of
    PubKeyCredential pkh -> Just pkh
    _ -> Nothing

logAllAssets :: Value.Value -> Contract Unit
logAllAssets val = do
  let tokens = Value.flattenNonAdaAssets val
  tokensStr <- foldMap assetToString tokens
  logInfo' tokensStr

assetToString :: (Value.CurrencySymbol /\ Value.TokenName /\ BigInt) -> Contract String
assetToString (cs /\ tn /\ quantity) = do

  let csString = currencySymbolToString cs
  tnStr <- liftContractM "Impossible to encode Token name" $ tokenNameToString tn
  pure $ "\nCurrency: " <> csString <> ", tokenName: " <> tnStr <> ", quantity: " <> toString quantity
