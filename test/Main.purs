module Test.Scaffold.Main (main) where

import Prelude

import Effect (Effect)
import Test.UnitTests as UnitTests

main :: Effect Unit
main = do
  UnitTests.main
