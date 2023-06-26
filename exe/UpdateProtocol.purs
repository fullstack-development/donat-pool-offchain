module CLI.UpdateProtocol.Main where

import Prelude

import Effect (Effect)
import Protocol.UpdateProtocol (runUpdateProtocol)

main :: Effect Unit
main = runUpdateProtocol
