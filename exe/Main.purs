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
-- import Protocol.Datum
-- import Contract.Value as Value
-- import Protocol.Models (Protocol)

main :: Effect Unit
main = StartProtocol.runStartProtocolTest
-- main = UpdateProtocol.runUpdateProtocol
-- main = CloseProtocol.runCloseProtocolTest
-- main = ConnectWallet.runConnectWallet

-- runGetProtocolInfo with parameter and not unit-type returning value
-- main :: Protocol -> Effect (Fiber (Tuple PProtocolDatum Value.Value))
-- main = ProtocolInfo.runGetProtocolInfo
