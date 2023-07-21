module Shared.OwnCredentials where

import Contract.Prelude

import Contract.Address (addressWithNetworkTagToBech32, AddressWithNetworkTag, PaymentPubKeyHash, StakePubKeyHash, getWalletAddressesWithNetworkTag, ownPaymentPubKeysHashes, ownStakePubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Data.Array as Array
import Data.Map as Map
import Info.UserData (UserInfo(..))
import Shared.Utxo (UtxoTuple, getNonCollateralUtxo)

newtype OwnCredentials = OwnCredentials
  { ownPkh :: PaymentPubKeyHash
  , ownSkh :: StakePubKeyHash
  , ownAddressWithNetworkTag :: AddressWithNetworkTag
  , ownUtxos :: (Map.Map TransactionInput TransactionOutputWithRefScript)
  , nonCollateralORef :: TransactionInput
  }

getOwnCreds :: Contract OwnCredentials
getOwnCreds = do
  (ownPkh /\ ownAddressWithNetworkTag) <- getOwnPkhAndAddress
  mbOwnSkh <- join <<< Array.head <$> ownStakePubKeysHashes
  ownSkh <- liftContractM "Failed to get own SKH" mbOwnSkh
  utxos <- utxosAt ownAddressWithNetworkTag >>= getNonCollateralUtxo
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array UtxoTuple))
  pure $ OwnCredentials
    { ownPkh: ownPkh
    , ownSkh: ownSkh
    , ownAddressWithNetworkTag: ownAddressWithNetworkTag
    , ownUtxos: utxos
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