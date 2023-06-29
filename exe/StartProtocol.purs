module CLI.StartProtocol.Main where

import Prelude

import Effect (Effect)
import Protocol.StartProtocol (runStartProtocol)

main :: Effect Unit
main = runStartProtocol
