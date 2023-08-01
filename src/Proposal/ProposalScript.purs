module Proposal.ProposalScript where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (PlutusData, toData)
import Contract.Scripts (Validator(..), PlutusScript, ApplyArgsError, applyArgs, validatorHash, ValidatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (singleton) as Array
import Effect.Exception (error)
import Ext.Contract.Value (runMkTokenName)
import Proposal.Model (PProposal)

foreign import proposalValidator :: String

proposalValidatorScript :: PProposal -> Contract Validator
proposalValidatorScript proposal = do
  script <- liftMaybe (error "Error decoding proposalValidator") do
    envelope <- decodeTextEnvelope proposalValidator
    plutusScriptV2FromEnvelope envelope
  res <- liftContractE $ mkProposalValidatorScript script proposal
  pure $ Validator res

mkProposalValidatorScript
  :: PlutusScript
  -> PProposal
  -> Either ApplyArgsError PlutusScript
mkProposalValidatorScript unappliedValidator proposal =
  let
    validatorArgs :: Array PlutusData
    validatorArgs = Array.singleton (toData proposal)
  in
    applyArgs unappliedValidator validatorArgs

getProposalValidatorHash :: PProposal -> Contract ValidatorHash
getProposalValidatorHash proposal = do
  validator <- proposalValidatorScript proposal
  pure $ validatorHash validator

proposalTokenName :: Contract Value.TokenName
proposalTokenName = runMkTokenName "ProposalId"

proposalVerTokenName :: Contract Value.TokenName
proposalVerTokenName = runMkTokenName "ProposalVerified"
