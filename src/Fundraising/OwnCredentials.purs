module Fundraising.OwnCredentials where

import Contract.Prelude

import Contract.Address (addressWithNetworkTagToBech32, AddressWithNetworkTag, PaymentPubKeyHash, StakePubKeyHash, getWalletAddressesWithNetworkTag, ownPaymentPubKeysHashes, ownStakePubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Data.Array (head) as Array
import Data.Map (Map)
import Shared.Utxo (getNonCollateralUtxo)
import Info.UserData (UserInfo(..))

newtype OwnCredentials = OwnCredentials
  { ownPkh :: PaymentPubKeyHash
  , ownSkh :: StakePubKeyHash
  , ownAddressWithNetworkTag :: AddressWithNetworkTag
  , ownUtxo :: (Map TransactionInput TransactionOutputWithRefScript)
  }

getOwnCreds :: Contract OwnCredentials
getOwnCreds = do
  (ownPkh /\ ownAddressWithNetworkTag) <- getOwnPkhAndAddress
  mbOwnSkh <- join <<< Array.head <$> ownStakePubKeysHashes
  ownSkh <- liftContractM "Failed to get own SKH" mbOwnSkh
  ownUtxo <- utxosAt ownAddressWithNetworkTag >>= getNonCollateralUtxo
  pure $ OwnCredentials
    { ownPkh: ownPkh
    , ownSkh: ownSkh
    , ownAddressWithNetworkTag: ownAddressWithNetworkTag
    , ownUtxo: ownUtxo
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