module Common.ConnectWallet where

import Contract.Prelude

import Contract.Address (Bech32String, addressToBech32, getWalletAddresses)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Data.Array (head) as Array
import Shared.NetworkData (NetworkParams)
import Shared.RunContract (runContractWithResult)

runConnectWallet :: (Bech32String -> Effect Unit) -> (String -> Effect Unit) -> NetworkParams -> Effect Unit
runConnectWallet onComplete onError networkParms = runContractWithResult onComplete onError networkParms contract

contract :: Contract Bech32String
contract = do
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  bech32Address <- addressToBech32 ownAddress
  logInfo' $ "User bech32 address: " <> show bech32Address
  pure bech32Address
