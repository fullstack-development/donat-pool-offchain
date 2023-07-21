module CLI.StartProtocol.Main where

import Prelude

import Effect (Effect)
import Protocol.StartProtocol (runStartSystem)

main :: Effect Unit
main = runStartSystem
