module Protocol.ProtocolScript
  ( protocolValidatorScript
  , getProtocolValidatorHash
  , protocolTokenName
  ) where

import Contract.Monad (Contract, liftContractE)
import Contract.Prelude (Either, bind, pure, ($))
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs, validatorHash, ValidatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)
import Protocol.Models (Protocol)
import Contract.PlutusData (PlutusData, toData)
import Data.Array (singleton) as Array
import Contract.Value as Value
import Shared.Helpers as Helpers

foreign import protocolValidator :: String

protocolValidatorScript :: Protocol -> Contract () Validator
protocolValidatorScript protocol = do
  script <- liftMaybe (error "Error decoding protocolValidator") do
    envelope <- decodeTextEnvelope protocolValidator
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkProtocolValidatorScript script protocol
  pure $ Validator res

mkProtocolValidatorScript
  :: PlutusScript
  -> Protocol
  -> Either ApplyArgsError PlutusScript
mkProtocolValidatorScript unappliedValidator protocol =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData protocol)
  in
    applyArgs unappliedValidator validatorArgs

getProtocolValidatorHash :: Protocol -> Contract () ValidatorHash
getProtocolValidatorHash protocol = do
  validator <- protocolValidatorScript protocol
  pure $ validatorHash validator

protocolTokenName :: forall (r :: Row Type). Contract r Value.TokenName
protocolTokenName = Helpers.mkTokenName "DonatPoolProtocol"
