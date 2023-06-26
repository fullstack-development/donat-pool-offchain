module Ext.Data.Either where

import Prelude
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Exception (throw)

eitherM ∷ forall l r. Show l => String -> Either l r -> Effect r
eitherM errMsg = either (\err -> throw (errMsg <> show err)) pure
