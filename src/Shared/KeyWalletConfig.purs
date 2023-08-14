module Shared.KeyWalletConfig where

import Contract.Prelude

import Contract.Config (PrivateStakeKeySource(..), testnetConfig)
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams, QueryBackendParams)
import Ctl.Internal.Wallet.Spec (PrivatePaymentKeySource(..), WalletSpec(..))
import Node.Path (FilePath)
import Shared.TestnetConfig (kupoConfig, ogmiosConfig)

privatePaymentKeyFilePath ∷ FilePath
privatePaymentKeyFilePath = "wallet/payment.skey"

privateStakeKeyFilePath ∷ FilePath
privateStakeKeyFilePath = "wallet/stake.skey"

keyWalletSpec ∷ WalletSpec
keyWalletSpec = UseKeys (PrivatePaymentKeyFile privatePaymentKeyFilePath) (Just $ PrivateStakeKeyFile privateStakeKeyFilePath)

testnetKeyWalletConfig :: ContractParams
testnetKeyWalletConfig = testnetConfig
  { backendParams = backendParams
  , walletSpec = Just keyWalletSpec
  , logLevel = Info
  }

backendParams ∷ QueryBackendParams
backendParams = mkCtlBackendParams
  { ogmiosConfig: ogmiosConfig
  , kupoConfig: kupoConfig
  }
