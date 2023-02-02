-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main, Contracts) where

import Contract.Prelude

import Common.ConnectWallet as ConnectWallet
import Effect.Aff (Fiber)
import Info.Protocol as ProtocolInfo
import Protocol.CloseProtocol as CloseProtocol
import Protocol.Models (Protocol)
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol
import Protocol.UserData (ProtocolConfigParams)

data Contracts = Contracts
  { connectWallet :: Effect Unit
  , startProtocol :: ProtocolConfigParams -> Effect (Fiber Protocol)
  , updateProtocol :: Protocol -> ProtocolConfigParams -> Effect (Fiber ProtocolConfigParams)
  , closeProtocol :: Protocol -> Effect Unit
  , getProtocolInfo :: Protocol -> Effect (Fiber ProtocolConfigParams)
  }

main :: Effect Contracts
main = pure $ Contracts
  { connectWallet: ConnectWallet.runConnectWallet
  , startProtocol: StartProtocol.runStartProtocol
  , updateProtocol: UpdateProtocol.runUpdateProtocol
  , closeProtocol: CloseProtocol.runCloseProtocolTest
  , getProtocolInfo: ProtocolInfo.runGetProtocolInfo
  }
