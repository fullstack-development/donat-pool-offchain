module Protocol.StartProtocol
  ( getGovernanceConstraints
  , initialGovernanceConf
  , initialProtocolConfigParams
  , runStartSystem
  , startSystem
  , startProtocol
  ) where

import Contract.Prelude

import Config.Fundraising (makeFundraisingConfig, writeFundraisingConfig)
import Config.Protocol (mapFromProtocolData, writeProtocolConfig)
import Contract.Address (addressToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (fromInt)
import Effect.Aff (launchAff_)
import Ext.Contract.Value (mkCurrencySymbol)
import Governance.Config (getGovTokenFromConfig)
import Governance.Datum (GovernanceDatum(..))
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceTokenName, governanceValidatorScript)
import Governance.UserData (StartGovernanceData(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolTokenName, protocolValidatorScript)
import Protocol.UserData (ProtocolConfigParams(..), ProtocolData, dataToProtocol, protocolToData)
import Shared.Config (mapFromProtocolConfigParams, writeDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.ScriptRef (mkFundraisingRefScript, mkGovernanceRefScript, mkProposalRefScript, mkProtocolRefScript, mkVerTokenPolicyRef)
import Shared.Tx (completeTx)

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
  , duration: fromInt 60
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
  mkVerTokenPolicyRef protocolData
  frConfig <- makeFundraisingConfig protocol
  liftEffect $ writeFundraisingConfig frConfig
  pure protocolData

startProtocol :: ProtocolConfigParams -> Contract ProtocolData
startProtocol params@(ProtocolConfigParams confParams) = do
  logInfo' "Running startDonatPool protocol contract"
  ownCreds@(OwnCredentials creds) <- getOwnCreds
  mp /\ cs <- mkCurrencySymbol (NFT.mintingPolicy creds.nonCollateralORef)
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
      , managerAddress: (unwrap creds.ownAddressWithNetworkTag).address
      , tokenOriginRef: creds.nonCollateralORef
      }
    protocolNftValue = Value.singleton cs protocolTn one
    paymentToProtocol = Value.lovelaceValueOf (fromInt 2000000) <> protocolNftValue

  (govConstraints /\ govLookups) <- getGovernanceConstraints protocol

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput creds.nonCollateralORef
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
        <> Lookups.unspentOutputs creds.ownUtxos
        <> Lookups.validator protocolValidator
        <> govLookups

  completeTx lookups constraints ownCreds

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
  (govCurrency /\ govTokenName) <- getGovTokenFromConfig
  tn <- governanceTokenName

  govValidatorHash <- getGovernanceValidatorHash protocol'
  govValidator <- governanceValidatorScript protocol'

  let
    initialGovernanceDatum = GovernanceDatum
      { quorum: govData.quorum
      , fee: govData.fee
      , govCurrency: govCurrency
      , govTokenName: govTokenName
      , duration: govData.duration
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
