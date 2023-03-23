module Common.ConnectWallet where

import Contract.Prelude

import Contract.Address (Bech32String, addressToBech32, getWalletAddresses)
import Contract.Config (testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM, runContract)
import Data.Array (head) as Array
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)

runConnectWallet :: (Bech32String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
runConnectWallet onComplete onError = runAff_ handler $
  runContract testnetNamiConfig contract
  where
  handler :: Either Error Bech32String -> Effect Unit
  handler (Right response) = onComplete response
  handler (Left err) = onError $ message err

contract :: Contract () Bech32String
contract = do
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  bech32Address <- addressToBech32 ownAddress
  logInfo' $ "User bech32 address: " <> show bech32Address
  pure bech32Address
