module Shared.MintingPolicy.GovernancePolicyScript where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, PlutusData, Z, genericToData, toData)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionInput)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.PubKeyHash (PaymentPubKeyHash)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Effect.Exception (error)
import Ext.Contract.Value (mkTokenName)

foreign import governancePolicy :: String

governanceMintingPolicy :: TransactionInput -> Contract MintingPolicy
governanceMintingPolicy =
  map PlutusMintingPolicy <<< mintGovernancePolicyScript

mintGovernancePolicyScript :: TransactionInput -> Contract PlutusScript
mintGovernancePolicyScript param = do
  script <- liftMaybe (error "Error decoding nftPolicy") do
    envelope <- decodeTextEnvelope governancePolicy
    plutusScriptV2FromEnvelope envelope
  liftContractE $ mkMintGovernancePolicy script param

mkMintGovernancePolicy
  :: PlutusScript
  -> TransactionInput
  -> Either ApplyArgsError PlutusScript
mkMintGovernancePolicy unappliedMintingPolicy param =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData param)
  in
    applyArgs unappliedMintingPolicy mintingPolicyArgs

governanceTokenName :: Contract Value.TokenName
governanceTokenName = liftContractM "Cannot make token name" <<< mkTokenName $ "DonatPool_governance_testnet"

data GovernanceTokensRedeemer = PMintGovernanceTokens BigInt PaymentPubKeyHash

derive instance Generic GovernanceTokensRedeemer _

instance
  HasPlutusSchema
    GovernanceTokensRedeemer
    ( "PMintGovernanceTokens" := PNil @@ Z
        :+ PNil
    )

instance ToData GovernanceTokensRedeemer where
  toData = genericToData