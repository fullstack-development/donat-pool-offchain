module Ext.Data.Either where

import Prelude
import Contract.Monad (Contract)
import Contract.Prelude (liftEffect)
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Exception (throw)

eitherM ∷ forall l r. Show l => String -> Either l r -> Effect r
eitherM errMsg = either (\err -> throw (errMsg <> show err)) pure

eitherContract :: forall l r. Show l => String -> Either l r -> Contract r
eitherContract errMsg = either (\err -> liftEffect <<< throw $ (errMsg <> show err)) pure
