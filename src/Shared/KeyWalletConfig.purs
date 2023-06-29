module Shared.KeyWalletConfig where

import Contract.Prelude

import Contract.Config (PrivateStakeKeySource(..), testnetConfig)
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Wallet.Spec (PrivatePaymentKeySource(..), WalletSpec(..))
import Node.Path (FilePath)
import Ctl.Internal.ServerConfig (ServerConfig, defaultOgmiosWsConfig)
import Data.UInt as UInt
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams, QueryBackendParams)

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

kupoConfig :: ServerConfig
kupoConfig =
  { port: UInt.fromInt 1442
  , host: "127.0.0.1"
  , secure: false
  , path: Nothing
  }

backendParams ∷ QueryBackendParams
backendParams = mkCtlBackendParams
  { ogmiosConfig: defaultOgmiosWsConfig
  , kupoConfig: kupoConfig
  }
