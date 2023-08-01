module Governance.GovernanceScript where

import Contract.Prelude
import Ext.Contract.Value (mkTokenName)

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs, validatorHash, ValidatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (singleton) as Array
import Effect.Exception (error)
import Protocol.Models (Protocol)

foreign import governanceValidator :: String

governanceValidatorScript :: Protocol -> Contract Validator
governanceValidatorScript protocol = do
  script <- liftMaybe (error "Error decoding governanceValidator") do
    envelope <- decodeTextEnvelope governanceValidator
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkGovernanceValidatorScript script protocol
  pure $ Validator res

mkGovernanceValidatorScript
  :: PlutusScript
  -> Protocol
  -> Either ApplyArgsError PlutusScript
mkGovernanceValidatorScript unappliedValidator protocol =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData protocol)
  in
    applyArgs unappliedValidator validatorArgs

getGovernanceValidatorHash :: Protocol -> Contract ValidatorHash
getGovernanceValidatorHash protocol = do
  validator <- governanceValidatorScript protocol
  pure $ validatorHash validator

governanceTokenName :: Contract Value.TokenName
governanceTokenName = liftContractM "Can't make governance thread token name" $ mkTokenName "DonatPoolGovernance"
