module Test.Utils where

import Prelude

import Effect.Aff (error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException)

assertTrue
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => String
  -> Boolean
  -> m Unit
assertTrue msg b = unless b $ liftEffect $ throwException $ error msg

