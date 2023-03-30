module Test.Scaffold.Main (main) where

import Prelude

import Effect (Effect)
import Test.UnitTests as UnitTests
import Test.Plutip as PlutipTests

main :: Effect Unit
main = do
  UnitTests.main
  PlutipTests.main
