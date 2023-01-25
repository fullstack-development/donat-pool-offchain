-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

-- import Contract.Config as Contract.Config
-- import Contract.Monad as Contract.Monad
-- import Scaffold as Scaffold
import Protocol.StartProtocol as StartProtocol
import Protocol.CloseProtocol as CloseProtocol

main :: Effect Unit
main =
  CloseProtocol.runCloseProtocolTest
--StartProtocol.runStartProtocolTest

-- Contract.Monad.launchAff_
--   $ void
--   $ Contract.Monad.runContract Contract.Config.testnetNamiConfig
--   $ Scaffold.contract
