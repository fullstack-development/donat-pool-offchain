module CLI.CloseProtocol.Main where

import Prelude

import Effect (Effect)
import Protocol.CloseProtocol (runCloseProtocol)

main :: Effect Unit
main = runCloseProtocol
