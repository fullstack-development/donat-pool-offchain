-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main, Contracts) where

import Contract.Prelude

import Common.ConnectWallet as ConnectWallet
import Info.Protocol as ProtocolInfo
import Protocol.CloseProtocol as CloseProtocol
import Protocol.Models (Protocol)
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol
import Protocol.UserData (ProtocolConfigParams)

data Contracts = Contracts
  { connectWallet :: Effect Unit
  , startProtocol :: (Protocol -> Effect Unit) -> (String -> Effect Unit) -> ProtocolConfigParams -> Effect Unit
  , updateProtocol :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> ProtocolConfigParams -> Effect Unit
  , closeProtocol :: Protocol -> Effect Unit
  , getProtocolInfo :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
  }

main :: Effect Contracts
main = pure $ Contracts
  { connectWallet: ConnectWallet.runConnectWallet
  , startProtocol: StartProtocol.runStartProtocol
  , updateProtocol: UpdateProtocol.runUpdateProtocol
  , closeProtocol: CloseProtocol.runCloseProtocolTest
  , getProtocolInfo: ProtocolInfo.runGetProtocolInfo
  }
