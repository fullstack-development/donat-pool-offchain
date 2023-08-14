module Shared.TestnetConfig where

import Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (testnetConfig)
import Contract.Prelude (log, (/\))
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams, QueryBackendParams)
import Ctl.Internal.ServerConfig (ServerConfig)
import Ctl.Internal.Wallet.Spec (WalletSpec(..))
import Data.Log.Level (LogLevel(Debug))
import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Exception (throw)
import Shared.NetworkData (NetworkWallet(..), WalletType(..))

mkNetworkWalletConfig :: NetworkWallet -> Effect ContractParams
mkNetworkWalletConfig (NetworkWallet { networkId, walletType }) = do
  log $ "NetworkId: " <> show networkId
  log $ "WalletType: " <> show walletType
  case (walletType /\ networkId) of
    (Nami /\ TestnetId) -> pure testnetNamiConfig
    (Flint /\ TestnetId) -> pure testnetFlintConfig
    (Lode /\ TestnetId) -> pure testnetLodeConfig
    (Eternl /\ TestnetId) -> pure testnetEternlConfig
    _ -> throw "Wallet/network configuration not implemented"

ogmiosConfig :: ServerConfig
ogmiosConfig =
  { port: UInt.fromInt 443
  , host: "ogmios.donat-pool.io"
  , secure: true
  , path: Nothing
  }

kupoConfig :: ServerConfig
kupoConfig =
  { port: UInt.fromInt 443
  , host: "kupo.donat-pool.io"
  , secure: true
  , path: Nothing
  }

testnetWalletConfig :: ContractParams
testnetWalletConfig = testnetConfig
  { backendParams = backParams
  , logLevel = Debug
  }

backParams :: QueryBackendParams
backParams = mkCtlBackendParams
  { ogmiosConfig: ogmiosConfig
  , kupoConfig: kupoConfig
  }

testnetNamiConfig :: ContractParams
testnetNamiConfig = testnetWalletConfig { walletSpec = Just ConnectToNami }

testnetGeroConfig :: ContractParams
testnetGeroConfig = testnetWalletConfig { walletSpec = Just ConnectToGero }

testnetFlintConfig :: ContractParams
testnetFlintConfig = testnetWalletConfig { walletSpec = Just ConnectToFlint }

testnetLodeConfig :: ContractParams
testnetLodeConfig = testnetWalletConfig { walletSpec = Just ConnectToLode }

testnetEternlConfig :: ContractParams
testnetEternlConfig = testnetWalletConfig { walletSpec = Just ConnectToEternl }

testnetNuFiConfig :: ContractParams
testnetNuFiConfig = testnetWalletConfig { walletSpec = Just ConnectToNuFi }
