module Shared.TestnetConfig where

import Prelude

import Contract.Config (defaultKupoServerConfig, defaultOgmiosWsConfig, testnetConfig)
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.ServerConfig (ServerConfig)
import Ctl.Internal.Wallet.Spec (WalletSpec(..))
import Data.Log.Level (LogLevel(Debug))
import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Effect (Effect)
import Web.HTML (window) as WEB
import Web.HTML.Location (hostname, protocol) as WEB
import Web.HTML.Window (location) as WEB

mkTestnetNamiConfig :: Effect ContractParams
mkTestnetNamiConfig = do
  location <- WEB.window >>= WEB.location
  host <- WEB.hostname location
  protocol <- WEB.protocol location
  let secure = (protocol == "https:" || protocol == "wss")
  pure $ testnetNamiConfig host secure

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

-- ogmiosProdWsConfig :: ServerConfig
-- ogmiosProdWsConfig =
--   { port: UInt.fromInt 1337
--   , host: "testnet.donat-pool.io"
--   , secure: true
--   , path: Nothing
--   }
-- ogmiosProdWsConfig :: ServerConfig
-- ogmiosProdWsConfig =
--   { port: UInt.fromInt 443
--   , host: "testnet.donat-pool.io"
--   , secure: true
--   , path: Just "ogmios"
--   }

ogmiosProdWsConfig :: String -> Boolean -> ServerConfig
ogmiosProdWsConfig host secure =
  { port: UInt.fromInt 1337
  , host: host
  , secure: secure
  , path: Nothing
  }

testnetNamiConfig :: String -> Boolean -> ContractParams
testnetNamiConfig host secure = testnetConfig
  { backendParams = mkCtlBackendParams
      { ogmiosConfig: if isProduction then ogmiosProdWsConfig host secure else defaultOgmiosWsConfig
      , kupoConfig: if isProduction then kupoProdConfig host secure else defaultKupoServerConfig
      }
  , walletSpec = Just ConnectToNami
  , logLevel = Debug
  }
  where
  isProduction = not $ host == "localhost"