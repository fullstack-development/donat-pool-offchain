-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

import Common.ConnectWallet as ConnectWallet
import Protocol.CloseProtocol as CloseProtocol
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol

-- uncomment to run getProtocolInfo
-- import Info.Protocol as ProtocolInfo
-- import Effect.Aff (Fiber)
-- import Protocol.Models (Protocol)
-- import Protocol.UserData (ProtocolConfigParams)

main :: Effect Unit
main = StartProtocol.runStartProtocolTest
-- main = UpdateProtocol.runUpdateProtocol
-- main = CloseProtocol.runCloseProtocolTest
-- main = ConnectWallet.runConnectWallet

-- runGetProtocolInfo with parameter and not unit-type returning value
-- main :: Protocol -> Effect (Fiber ProtocolConfigParams)
-- main = ProtocolInfo.runGetProtocolInfo
