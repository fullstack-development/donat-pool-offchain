module Common.ConnectWallet where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, ownPaymentPubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (ConfigParams, Contract, launchAff_, liftContractM, runContract)
import Data.Array (head) as Array
import Contract.Config (testnetNamiConfig)

runConnectWallet :: Effect Unit
runConnectWallet = connectWallet testnetNamiConfig

connectWallet :: ConfigParams () -> Effect Unit
connectWallet baseConfig = launchAff_ $ do
  runContract baseConfig contract

contract :: Contract () PaymentPubKeyHash
contract = do
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  logInfo' $ show ownPkh
  pure ownPkh
