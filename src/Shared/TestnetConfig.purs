module Shared.TestnetConfig where

import Prelude

import Contract.Config (testnetConfig)
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.ServerConfig (ServerConfig)
import Ctl.Internal.Wallet.Spec (WalletSpec(..))
import Data.Log.Level (LogLevel(Debug))
import Data.Maybe (Maybe(Just, Nothing))
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
  let secure = protocol == "https:"
  pure $ testnetNamiConfig host secure

kupoConfig :: String -> Boolean -> ServerConfig
kupoConfig host secure =
  { port: UInt.fromInt 4008
  , host: "127.0.0.1"
  , secure: false
  , path: Just "kupo"
  }

ogmiosWsConfig :: String -> Boolean -> ServerConfig
ogmiosWsConfig host secure =
  { port: UInt.fromInt 80
  , host: host
  , secure: secure
  , path: Just "ogmios"  -- TODO: only for host != localhost
  }

testnetNamiConfig :: String -> Boolean -> ContractParams
testnetNamiConfig host secure = testnetConfig
  { backendParams = mkCtlBackendParams
      { ogmiosConfig: ogmiosWsConfig host secure
      , kupoConfig: kupoConfig host secure
      }
  , walletSpec = Just ConnectToNami
  , logLevel = Debug
  }
