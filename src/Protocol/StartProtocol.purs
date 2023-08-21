module Protocol.StartProtocol where

import Contract.Prelude

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
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolTokenName, protocolValidatorScript)
import Protocol.UserData (ProtocolConfigParams(..), ProtocolData, protocolToData)
import Shared.Config (mapFromProtocolConfigParams, writeDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.ScriptRef (mkFundraisingRefScript, mkProtocolRefScript, mkVerTokenPolicyRef)
import Shared.Tx (completeTx)

initialProtocolConfigParams ∷ ProtocolConfigParams
initialProtocolConfigParams = ProtocolConfigParams
  { minAmountParam: fromInt 50000000
  , maxAmountParam: fromInt 1000000000
  , minDurationParam: fromInt 5
  , maxDurationParam: fromInt 86400
  , protocolFeeParam: fromInt 10
  }

runStartSystem :: Effect Unit
runStartSystem = launchAff_ $ do
  runContract testnetKeyWalletConfig (startSystem initialProtocolConfigParams)

startSystem :: ProtocolConfigParams -> Contract ProtocolData
startSystem params = do
  protocolData <- startProtocol params
  mkProtocolRefScript protocolData
  mkFundraisingRefScript protocolData
  mkVerTokenPolicyRef protocolData
  pure protocolData

startProtocol :: ProtocolConfigParams -> Contract ProtocolData
startProtocol params@(ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  logInfo' "Running startDonatPool protocol contract"
  ownCreds@(OwnCredentials creds) <- getOwnCreds
  mp /\ cs <- mkCurrencySymbol (NFT.mintingPolicy creds.nonCollateralORef)
  tn <- protocolTokenName
  let
    protocol = Protocol
      { protocolCurrency: cs
      , protocolTokenName: tn
      }
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolValidator <- protocolValidatorScript protocol

  let
    initialProtocolDatum = PProtocolDatum
      { minAmount: minAmountParam
      , maxAmount: maxAmountParam
      , minDuration: minDurationParam
      , maxDuration: maxDurationParam
      , protocolFee: protocolFeeParam
      , managerAddress: (unwrap creds.ownAddressWithNetworkTag).address
      , tokenOriginRef: creds.nonCollateralORef
      }
    nftValue = Value.singleton cs tn one
    paymentToProtocol = Value.lovelaceValueOf (fromInt 2000000) <> nftValue

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput creds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft tn)
          nftValue
        <> Constraints.mustPayToScriptAddress
          protocolValidatorHash
          (ScriptCredential protocolValidatorHash)
          (Datum $ toData initialProtocolDatum)
          Constraints.DatumInline
          paymentToProtocol

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs creds.ownUtxos
        <> Lookups.validator protocolValidator

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
