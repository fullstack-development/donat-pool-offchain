module CLI.StartProtocol where

import Prelude

import Effect (Effect)
import Protocol.StartProtocol (runStartProtocol)

main :: Effect Unit
main = runStartProtocol
