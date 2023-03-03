module Fundraising.FundraisingScript where

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.Prelude
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs, validatorHash, ValidatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)
import Contract.PlutusData (PlutusData, toData)
import Data.Array (singleton) as Array
import Contract.Value as Value
import Shared.Helpers as Helpers
import Fundraising.Models (Fundraising)

foreign import fundraisingValidator :: String

fundraisingValidatorScript :: Fundraising -> Contract () Validator
fundraisingValidatorScript fundraising = do
  script <- liftMaybe (error "Error decoding fundraisingValidator") do
    envelope <- decodeTextEnvelope fundraisingValidator
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkFundraisingValidatorScript script fundraising
  pure $ Validator res

mkFundraisingValidatorScript :: PlutusScript -> Fundraising -> Either ApplyArgsError PlutusScript
mkFundraisingValidatorScript unappliedValidator fundraising =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData fundraising)
  in
    applyArgs unappliedValidator validatorArgs

getFundraisingValidatorHash :: Fundraising -> Contract () ValidatorHash
getFundraisingValidatorHash fundraising = do
  validator <- fundraisingValidatorScript fundraising
  pure $ validatorHash validator

getFundraisingTokenName :: forall (r :: Row Type). Contract r Value.TokenName
getFundraisingTokenName = liftContractM "Cannot make Fundraising token name" $ fundraisingTokenName

fundraisingTokenName :: Maybe Value.TokenName
fundraisingTokenName = Helpers.mkTokenName "FundraisingThreadToken"
