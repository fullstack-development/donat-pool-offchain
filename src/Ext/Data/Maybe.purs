module Ext.Data.Maybe where

import Prelude
import Data.Maybe (maybe, Maybe)
import Effect.Exception (throw)
import Effect (Effect)

maybeM :: forall a. String -> Maybe a -> Effect a
maybeM errMsg = maybe (throw errMsg) pure