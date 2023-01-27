-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

import Protocol.CloseProtocol as CloseProtocol
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol


main :: Effect Unit
-- main = StartProtocol.runStartProtocolTest
main = UpdateProtocol.runUpdateProtocol
-- main = CloseProtocol.runCloseProtocolTest

-- Contract.Monad.launchAff_
--   $ void
--   $ Contract.Monad.runContract Contract.Config.testnetNamiConfig
--   $ Scaffold.contract
