module MintingPolicy.ProposalMinting where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, PlutusData, S, Z, genericToData, toData)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionInput)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Effect.Exception (error)

foreign import proposalPolicy :: String

proposalMintingPolicy :: TransactionInput -> Contract MintingPolicy
proposalMintingPolicy =
  map PlutusMintingPolicy <<< mintProposalPolicyScript

mintProposalPolicyScript :: TransactionInput -> Contract PlutusScript
mintProposalPolicyScript param = do
  script <- liftMaybe (error "Error decoding nftPolicy") do
    envelope <- decodeTextEnvelope proposalPolicy
    plutusScriptV2FromEnvelope envelope
  liftContractE $ mkMintProposalPolicy script param

mkMintProposalPolicy
  :: PlutusScript
  -> TransactionInput
  -> Either ApplyArgsError PlutusScript
mkMintProposalPolicy unappliedMintingPolicy param =
  let
    mintingPolicyArgs :: Array PlutusData
    mintingPolicyArgs = Array.singleton (toData param)
  in
    applyArgs unappliedMintingPolicy mintingPolicyArgs

type PThreadTokenName = Value.TokenName
type PVoteTokenName = Value.TokenName
type PVerCs = Value.CurrencySymbol
type PVote = BigInt -- 1 - for, 0 - against
type PAmount = BigInt

data PProposalPolicyRedeemer
  = PMintThreadToken PThreadTokenName
  | PBurnThreadToken PThreadTokenName
  | PMintVoteToken PVerCs
  | PBurnVoteToken PVoteTokenName PVerCs

derive instance Generic PProposalPolicyRedeemer _

instance
  HasPlutusSchema
    PProposalPolicyRedeemer
    ( "PMintThreadToken" := PNil @@ Z
        :+ "PBurnThreadToken"
        := PNil
        @@ (S Z)
        :+ "PMintVoteToken"
        := PNil
        @@ (S (S Z))
        :+ "PBurnVoteToken"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

instance ToData PProposalPolicyRedeemer where
  toData = genericToData
