module Protocol.StartProtocol
  ( getGovernanceConstraints
  , initialGovernanceConf
  , initialProtocolConfigParams
  , runStartSystem
  , startSystem
  , startProtocol
  )
  where

import Contract.Prelude
import Governance.UserData (StartGovernanceData(..))
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Config.Protocol (mapFromProtocolData, writeProtocolConfig)
import Contract.Address (addressToBech32, getNetworkId, getWalletAddressesWithNetworkTag, validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract, liftContractM, liftedM, liftedE)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array (head) as Array
import Data.BigInt (fromInt)
import Effect.Aff (launchAff_)
import Ext.Contract.Value (mkCurrencySymbol)
import Governance.Config (getGovCurrencySymbolFromConfig)
import Governance.Datum (GovernanceDatum(..))
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceTokenName, governanceValidatorScript)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolTokenName, protocolValidatorScript)
import Protocol.UserData (ProtocolConfigParams(..), ProtocolData, dataToProtocol, protocolToData)
import Shared.Config (mapFromProtocolConfigParams, writeDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.ScriptRef (mkFundraisingRefScript, mkGovernanceRefScript, mkProposalRefScript, mkProtocolRefScript)


initialProtocolConfigParams ∷ ProtocolConfigParams
initialProtocolConfigParams = ProtocolConfigParams
  { minAmountParam: fromInt 50000000
  , maxAmountParam: fromInt 1000000000
  , minDurationParam: fromInt 5
  , maxDurationParam: fromInt 86400
  , protocolFeeParam: fromInt 10
  }

initialGovernanceConf :: StartGovernanceData
initialGovernanceConf = StartGovernanceData
  { quorum: fromInt 60
  , fee: fromInt 10_000_000
  }

runStartSystem :: Effect Unit
runStartSystem = launchAff_ $ do
  runContract testnetKeyWalletConfig (startSystem initialProtocolConfigParams)

startSystem :: ProtocolConfigParams -> Contract ProtocolData
startSystem params = do
  protocolData <- startProtocol params
  mkProtocolRefScript protocolData 
  mkFundraisingRefScript protocolData
  protocol <- dataToProtocol protocolData
  mkGovernanceRefScript protocol
  mkProposalRefScript protocol
  pure protocolData

startProtocol :: ProtocolConfigParams -> Contract ProtocolData
startProtocol params@(ProtocolConfigParams confParams) = do
  logInfo' "Running startDonatPool protocol contract"
  OwnCredentials ownCreds <- getOwnCreds
  
  mp /\ cs <- mkCurrencySymbol (NFT.mintingPolicy ownCreds.nonCollateralORef)
  protocolTn <- protocolTokenName
  let
    protocol = Protocol
      { protocolCurrency: cs
      , protocolTokenName: protocolTn
      }
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolValidator <- protocolValidatorScript protocol

  let
    initialProtocolDatum = PProtocolDatum
      { minAmount: confParams.minAmountParam
      , maxAmount: confParams.maxAmountParam
      , minDuration: confParams.minDurationParam
      , maxDuration: confParams.maxDurationParam
      , protocolFee: confParams.protocolFeeParam
      , managerAddress: (unwrap ownCreds.ownAddressWithNetworkTag).address
      , tokenOriginRef: ownCreds.nonCollateralORef
      }
    protocolNftValue = Value.singleton cs protocolTn one
    paymentToProtocol = Value.lovelaceValueOf (fromInt 2000000) <> protocolNftValue

  (govConstraints /\ govLookups) <- getGovernanceConstraints protocol

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft protocolTn)
          protocolNftValue
        <> Constraints.mustPayToScriptAddress
          protocolValidatorHash
          (ScriptCredential protocolValidatorHash)
          (Datum $ toData initialProtocolDatum)
          Constraints.DatumInline
          paymentToProtocol
        <> govConstraints

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs ownCreds.ownUtxos
        <> Lookups.validator protocolValidator
        <> govLookups

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <- liftedM "Failed to get own address with Network Tag" $ Array.head <$> getWalletAddressesWithNetworkTag
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId

  logInfo' $ "Current protocol: " <> show protocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  bech32Address <- addressToBech32 protocolAddress
  logInfo' $ "Current protocol address: " <> show bech32Address
  logInfo' "Transaction submitted successfully"

  protocolData <- protocolToData protocol

  let protocolConfig = mapFromProtocolData protocolData
  liftEffect $ writeProtocolConfig protocolConfig

  let donatPoolConfig = mapFromProtocolConfigParams params
  liftEffect $ writeDonatPoolConfig donatPoolConfig

  pure protocolData

getGovernanceConstraints :: Protocol -> Contract (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
getGovernanceConstraints protocol'@(Protocol protocol) = do
  let StartGovernanceData govData = initialGovernanceConf
  govCurrency <- getGovCurrencySymbolFromConfig
  tn <- governanceTokenName

  govValidatorHash <- getGovernanceValidatorHash protocol'
  govValidator <- governanceValidatorScript protocol'

  let
    initialGovernanceDatum = GovernanceDatum
      { quorum: govData.quorum
      , fee: govData.fee
      , govCurrency: govCurrency
      }
    nftValue = Value.singleton protocol.protocolCurrency tn one
    paymentToGov = Value.lovelaceValueOf (fromInt 2000000) <> nftValue

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft tn)
          nftValue
        <> Constraints.mustPayToScriptAddress
          govValidatorHash
          (ScriptCredential govValidatorHash)
          (Datum $ toData initialGovernanceDatum)
          Constraints.DatumInline
          paymentToGov

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.validator govValidator

  networkId <- getNetworkId
  governanceAddress <-
    liftContractM "Impossible to get governance script address" $ validatorHashBaseAddress networkId govValidatorHash
  bech32Address <- addressToBech32 governanceAddress
  logInfo' $ "Current governance address: " <> show bech32Address

  pure (constraints /\ lookups)
