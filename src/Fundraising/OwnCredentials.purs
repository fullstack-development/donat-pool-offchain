module Fundraising.OwnCredentials where

import Contract.Prelude

import Contract.Address (Address, PaymentPubKeyHash, StakePubKeyHash, getWalletAddresses, ownPaymentPubKeysHashes, ownStakePubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Utxos (utxosAt)
import Data.Array (head) as Array
import Shared.Helpers (getNonCollateralUtxo)
import Data.Map (Map)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )

newtype OwnCredentials = OwnCredentials
  { ownPkh :: PaymentPubKeyHash
  , ownSkh :: StakePubKeyHash
  , ownAddress :: Address
  , ownUtxo :: (Map TransactionInput TransactionOutputWithRefScript)
  }

getOwnCreds :: Contract () OwnCredentials
getOwnCreds = do
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  mbOwnSkh <- join <<< Array.head <$> ownStakePubKeysHashes
  ownSkh <- liftContractM "Failed to get own SKH" mbOwnSkh
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  ownUtxo <- utxosAt ownAddress >>= getNonCollateralUtxo
  pure $ OwnCredentials
    { ownPkh: ownPkh
    , ownSkh: ownSkh
    , ownAddress: ownAddress
    , ownUtxo: ownUtxo
    }

