module MintingPolicy.VerTokenMinting where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (PlutusData, toData)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (singleton) as Array
import Effect.Exception (error)
import Protocol.Models (Protocol)
import Ext.Contract.Value (runMkTokenName)

foreign import verTokenPolicy :: String

mintingPolicy :: Protocol -> Contract MintingPolicy
mintingPolicy = map PlutusMintingPolicy <<< mintVerTokenScript

mintVerTokenScript :: Protocol -> Contract PlutusScript
mintVerTokenScript protocol = do
  script <- liftMaybe (error "Error decoding nftPolicy") do
    envelope <- decodeTextEnvelope verTokenPolicy
    plutusScriptV2FromEnvelope envelope
  liftContractE $ mkMintVerTokenPolicy script protocol

mkMintVerTokenPolicy :: PlutusScript -> Protocol -> Either ApplyArgsError PlutusScript
mkMintVerTokenPolicy unappliedPolicy protocol =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData protocol)
  in
    applyArgs unappliedPolicy mintingPolicyArgs

verTokenName :: Contract Value.TokenName
verTokenName = runMkTokenName "VerificationToken"

feePoolVerTokenName :: Contract Value.TokenName
feePoolVerTokenName = runMkTokenName "FeePoolInfoVerToken"
