module Shared.TestnetConfig where

import Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (defaultOgmiosWsConfig, testnetConfig)
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
import Web.HTML (window) as WEB
import Web.HTML.Location (hostname, protocol) as WEB
import Web.HTML.Window (location) as WEB

mkNetworkWalletConfig :: NetworkWallet -> Effect ContractParams
mkNetworkWalletConfig (NetworkWallet { networkId, walletType }) = do
  log $ "NetworkId: " <> show networkId
  log $ "WalletType: " <> show walletType
  location <- WEB.window >>= WEB.location
  host <- WEB.hostname location
  protocol <- WEB.protocol location
  let secure = (protocol == "https:" || protocol == "wss")
  case (walletType /\ networkId) of
    (Nami /\ TestnetId) -> pure $ testnetNamiConfig host secure
    (Flint /\ TestnetId) -> pure $ testnetFlintConfig host secure
    (Lode /\ TestnetId) -> pure $ testnetLodeConfig host secure
    (Eternl /\ TestnetId) -> pure $ testnetEternlConfig host secure
    _ -> throw "Wallet/network configuration not implemented"

kupoProdConfig :: String -> Boolean -> ServerConfig
kupoProdConfig host secure =
  let
    port = if secure then 443 else 80
  in
    { port: UInt.fromInt port
    , host: host
    , secure: secure
    , path: Just "kupo"
    }

ogmiosProdWsConfig :: ServerConfig
ogmiosProdWsConfig =
  { port: UInt.fromInt 443
  , host: "ogmios.donat-pool.io"
  , secure: true
  , path: Nothing
  }

kupoConfig :: ServerConfig
kupoConfig =
  { port: UInt.fromInt 1442
  , host: "localhost"
  , secure: false
  , path: Nothing
  }

testnetWalletConfig :: String -> Boolean -> ContractParams
testnetWalletConfig host secure = testnetConfig
  { backendParams = backParams host secure
  , logLevel = Debug
  }

backParams :: String -> Boolean -> QueryBackendParams
backParams host secure = mkCtlBackendParams
  { ogmiosConfig: if isProduction then ogmiosProdWsConfig else defaultOgmiosWsConfig
  , kupoConfig: kupoConfig
  }
  where
  isProduction = not $ host == "localhost"

testnetNamiConfig :: String -> Boolean -> ContractParams
testnetNamiConfig host secure = (testnetWalletConfig host secure) { walletSpec = Just ConnectToNami }

testnetGeroConfig :: String -> Boolean -> ContractParams
testnetGeroConfig host secure = (testnetWalletConfig host secure) { walletSpec = Just ConnectToGero }

testnetFlintConfig :: String -> Boolean -> ContractParams
testnetFlintConfig host secure = (testnetWalletConfig host secure) { walletSpec = Just ConnectToFlint }

testnetLodeConfig :: String -> Boolean -> ContractParams
testnetLodeConfig host secure = (testnetWalletConfig host secure) { walletSpec = Just ConnectToLode }

testnetEternlConfig :: String -> Boolean -> ContractParams
testnetEternlConfig host secure = (testnetWalletConfig host secure) { walletSpec = Just ConnectToEternl }

testnetNuFiConfig :: String -> Boolean -> ContractParams
testnetNuFiConfig host secure = (testnetWalletConfig host secure) { walletSpec = Just ConnectToNuFi }
