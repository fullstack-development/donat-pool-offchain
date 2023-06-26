module CLI.UpdateProtocol where

import Prelude

import Effect (Effect)
import Protocol.UpdateProtocol (runUpdateProtocol)

main :: Effect Unit
main = runUpdateProtocol
