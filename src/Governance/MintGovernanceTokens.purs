module Governance.MintGovernanceTokens where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)
import Ext.Contract.Value (currencySymbolToString, mkCurrencySymbol, tokenNameToString)
import Shared.MintingPolicy.GovernancePolicyScript (GovernanceTokensRedeemer(..), governanceMintingPolicy, governanceTokenName)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.RunContract (runContractWithResult)
import Shared.Tx (completeTx, toRedeemer)

-- NOTE: Dont' forget to copy governance token currency from logs to conf/governance.conf manually
runMintGovernanceTokens :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> NetworkParams -> Effect Unit
runMintGovernanceTokens onComplete onError networkParams = do
  runContractWithResult onComplete onError networkParams (mintGovernanceTokens $ fromInt 50000)

mintGovernanceTokens :: BigInt -> Contract Unit
mintGovernanceTokens amount = do
  logInfo' "Minting governance tokens"

  ownCreds'@(OwnCredentials ownCreds) <- getOwnCreds
  governanceMp /\ governanceCs <- mkCurrencySymbol (governanceMintingPolicy ownCreds.nonCollateralORef)
  governanceTn <- governanceTokenName
  let governanceNft = Value.singleton governanceCs governanceTn amount
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer (toRedeemer $ PMintGovernanceTokens) governanceNft
        <> Constraints.mustPayToPubKeyAddress ownCreds.ownPkh ownCreds.ownSkh governanceNft
        <> Constraints.mustBeSignedBy ownCreds.ownPkh

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy governanceMp
        <> Lookups.unspentOutputs ownCreds.ownUtxos

  completeTx lookups constraints ownCreds'

  let governanceCurrencyString = currencySymbolToString governanceCs
  governanceTnString <- liftContractM "Impossible to decode Protocol token name" $ tokenNameToString governanceTn

  let
    governanceConfig =
      { governanceCurrency: governanceCurrencyString
      , governanceTokenName: governanceTnString
      }

  logInfo' $ "Governance tokens minted successfully: " <> show governanceConfig
