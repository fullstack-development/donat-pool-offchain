module Governance.MintGovernanceTokens where

import Contract.Prelude
import Ext.Data.Maybe (maybeC)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)

import Contract.Address (PaymentPubKeyHash(..), toPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address (addressFromBech32)
import Data.BigInt (BigInt, fromInt)
import Ext.Contract.Value (currencySymbolToString, mkCurrencySymbol, tokenNameToString)
import Governance.Config (writeGovernanceConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.MintingPolicy.GovernancePolicyScript (GovernanceTokensRedeemer(..), governanceMintingPolicy, governanceTokenName)
import Shared.Tx (completeTx)

runMintGovernanceTokens :: Aff Unit
runMintGovernanceTokens = runContract testnetKeyWalletConfig (mintGovernanceTokens $ fromInt 5000)

mintGovernanceTokens :: BigInt -> Contract Unit
mintGovernanceTokens amount = do
  logInfo' "Minting governance tokens"

  ownCreds'@(OwnCredentials ownCreds) <- getOwnCreds
  governanceMp /\ governanceCs <- mkCurrencySymbol (governanceMintingPolicy ownCreds.nonCollateralORef)
  governanceTn <- governanceTokenName
  let governanceNft = Value.singleton governanceCs governanceTn amount
  distributionPkh <- getDistributionPkh
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer (Redeemer $ toData $ PMintGovernanceTokens amount ownCreds.ownPkh) governanceNft
        <> Constraints.mustPayToPubKey distributionPkh governanceNft
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

  liftEffect $ writeGovernanceConfig governanceConfig
  logInfo' "Governance tokens minted successfully."

  logInfo' "Governance tokens successfully sent to discribution address."

getDistributionPkh :: Contract PaymentPubKeyHash
getDistributionPkh = do
  let bech32Addr = "addr_test1qzusxk7w3zqxm2frq68ctc8wwndv03vng9qexy0mz0dn7fz4wlsygw0hkq07nk9cw52efu3eccymjta0lpwuygp2tn6q3w78nd"
  address <- liftContractM "Cannot make governance tokens distribution address" $ addressFromBech32 bech32Addr >>= toPlutusAddress
  pkh <- maybeC "Can't make pkh from governance tokens dictribution address" $ PaymentPubKeyHash <$> toPubKeyHash address
  pure pkh
