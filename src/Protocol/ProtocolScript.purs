module Protocol.ProtocolScript
  ( protocolValidatorScript
  )
  where


import Contract.Monad (Contract, liftContractE)
import Contract.Prelude (Either, bind, pure, ($))
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)
import Protocol.Models (PProtocol)
import Contract.PlutusData (PlutusData, toData)
import Data.Array (singleton) as Array

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
