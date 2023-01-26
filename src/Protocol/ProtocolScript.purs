module Protocol.ProtocolScript
  ( protocolValidatorScript
  , getProtocolValidatorHash
  , protocolTokenName
  ) where

import Contract.Monad (Contract, liftContractE)
import Contract.Prelude (Either, bind, pure, ($))
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)
import Protocol.Models (PProtocol)
import Contract.PlutusData (PlutusData, toData)
import Data.Array (singleton) as Array
import Contract.Scripts (validatorHash, ValidatorHash)
import Contract.Value as Value
import Shared.Helpers as Helpers

foreign import protocolValidator :: String

protocolValidatorScript :: PProtocol -> Contract () Validator
protocolValidatorScript protocol = do
  script <- liftMaybe (error "Error decoding protocolValidator") do
    envelope <- decodeTextEnvelope protocolValidator
    plutusScriptV1FromEnvelope envelope
  res <- liftContractE $ mkProtocolValidatorScript script protocol
  pure $ Validator res

mkProtocolValidatorScript
  :: PlutusScript
  -> PProtocol
  -> Either ApplyArgsError PlutusScript
mkProtocolValidatorScript unappliedMintingPolicy protocol =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData protocol)
  in
    applyArgs unappliedMintingPolicy mintingPolicyArgs

getProtocolValidatorHash :: PProtocol -> Contract () ValidatorHash
getProtocolValidatorHash protocol = do
  validator <- protocolValidatorScript protocol
  pure $ validatorHash validator

protocolTokenName :: forall (r :: Row Type). Contract r Value.TokenName
protocolTokenName = Helpers.mkTokenName "DonatPoolProtocol"