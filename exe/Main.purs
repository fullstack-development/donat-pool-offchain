-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main) where

import Contract.Prelude

import NftMinting as NftMinting

main :: Effect Unit
main = NftMinting.main
