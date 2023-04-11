module Fundraising.OwnCredentials where

import Contract.Prelude

import Contract.Address (AddressWithNetworkTag, PaymentPubKeyHash, StakePubKeyHash, getWalletAddressesWithNetworkTag, ownPaymentPubKeysHashes, ownStakePubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Data.Array (head) as Array
import Data.Map (Map)
import Shared.Helpers (getNonCollateralUtxo)

newtype OwnCredentials = OwnCredentials
  { ownPkh :: PaymentPubKeyHash
  , ownSkh :: StakePubKeyHash
  , ownAddressWithNetworkTag :: AddressWithNetworkTag
  , ownUtxo :: (Map TransactionInput TransactionOutputWithRefScript)
  }

getOwnCreds :: Contract OwnCredentials
getOwnCreds = do
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  mbOwnSkh <- join <<< Array.head <$> ownStakePubKeysHashes
  ownSkh <- liftContractM "Failed to get own SKH" mbOwnSkh
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddressWithNetworkTag <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddressesWithNetworkTag
  ownUtxo <- utxosAt ownAddressWithNetworkTag >>= getNonCollateralUtxo
  pure $ OwnCredentials
    { ownPkh: ownPkh
    , ownSkh: ownSkh
    , ownAddressWithNetworkTag: ownAddressWithNetworkTag
    , ownUtxo: ownUtxo
    }

