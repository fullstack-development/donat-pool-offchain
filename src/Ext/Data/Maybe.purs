module Ext.Data.Maybe where

import Prelude

import Contract.Monad (Contract)
import Data.Maybe (maybe, Maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

maybeM :: forall a. String -> Maybe a -> Effect a
maybeM errMsg = maybe (throw errMsg) pure

maybeC :: forall a. String -> Maybe a -> Contract a
maybeC errMsg = maybe (liftEffect $ throw errMsg) pure
