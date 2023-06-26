module CLI.CloseProtocol where

import Prelude

import Effect (Effect)
import Protocol.CloseProtocol (runCloseProtocol)

main :: Effect Unit
main = runCloseProtocol
