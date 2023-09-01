module Ext.Contract.Script where

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prelude
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.ToData (class ToData)
import Data.Array (singleton) as Array
import Effect.Exception (error)

validatorScriptWithArg :: forall arg. ToData arg => String -> arg -> Contract Validator
validatorScriptWithArg encodedScript arg = do
  script <- liftMaybe (error "Error decoding validator script") do
    envelope <- decodeTextEnvelope encodedScript
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkValidatorScriptWithArgs script arg
  pure $ Validator res

mkValidatorScriptWithArgs :: forall arg. ToData arg => PlutusScript -> arg -> Either ApplyArgsError PlutusScript
mkValidatorScriptWithArgs unappliedValidator arg =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData arg)
  in
    applyArgs unappliedValidator validatorArgs
