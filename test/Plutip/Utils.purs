module Test.Plutip.Utils where

import Prelude

import Data.Either (Either(..))
import Effect.Exception (Error, message)

isExpectedError :: forall a. String -> Either Error a -> Boolean
isExpectedError expMsg (Left err) = message err == expMsg
isExpectedError _ _ = false
