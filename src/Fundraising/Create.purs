module Fundraising.Create where

import Contract.Prelude

import Contract.Address (addressToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Chain (currentTime)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Redeemer(Redeemer), Datum(Datum), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicyHash, mintingPolicyHash)
import Contract.Time (POSIXTime(..))
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (byteArrayFromAscii)
import Data.BigInt (fromInt, toString)
import Data.Lens (view)
import Data.String (take)
import Effect.Exception (throw)
import Ext.Contract.Time (addTimes)
import Ext.Contract.Value (currencySymbolToString, mkCurrencySymbol)
import Ext.Seriaization.Key (pkhToBech32M)
import Fundraising.Datum (PFundraisingDatum(..), titleLength)
import Fundraising.FundraisingScript (fundraisingTokenNameString, getFundraisingTokenName, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.UserData (CreateFundraisingParams(..))
import Info.UserData (FundraisingInfo(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import MintingPolicy.VerTokenRedeemers (PVerTokenRedeemer(..))
import Protocol.Datum (_protocolFee, _minDuration, _maxDuration, _minAmount, _maxAmount, _managerAddress)
import Protocol.Models (PFundriseConfig(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.Duration (durationToMinutes, minutesToPosixTime)
import Shared.MinAda (minAdaValue)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.RunContract (runContractWithResult)
import Shared.Tx (completeTx)

runCreateFundraising
  :: (FundraisingInfo -> Effect Unit)
  -> (String -> Effect Unit)
  -> ProtocolData
  -> NetworkParams
  -> CreateFundraisingParams
  -> Effect Unit
runCreateFundraising onComplete onError protocolData networkParams funraisingParams = do
  runContractWithResult onComplete onError networkParams (contract protocolData funraisingParams)

contract :: ProtocolData -> CreateFundraisingParams -> Contract FundraisingInfo
contract protocolData (CreateFundraisingParams { title, amount, duration }) = do
  logInfo' "Running Create Fundraising contract"
  protocol <- dataToProtocol protocolData

  ownCreds@(OwnCredentials creds) <- getOwnCreds
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol

  nftMp /\ nftCs <- mkCurrencySymbol (NFT.mintingPolicy creds.nonCollateralORef)
  nftTn <- getFundraisingTokenName

  verTokenMp /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTn <- VerToken.verTokenName
  let
    verTokenPolicyHash :: MintingPolicyHash
    verTokenPolicyHash = mintingPolicyHash verTokenMp

  let
    minAmount = view _minAmount protocolInfo.pDatum
    maxAmount = view _maxAmount protocolInfo.pDatum
    targetAmount = fromInt amount * fromInt 1_000_000

  when (targetAmount < minAmount) $ liftEffect $ throw ("Fundraising amount too small. It must be greater than " <> toString minAmount <> ".")
  when (targetAmount > maxAmount) $ liftEffect $ throw ("Fundraising amount too big. It must be less than " <> toString maxAmount <> ".")

  let
    minDurationMinutes = view _minDuration protocolInfo.pDatum
    maxDurationMinutes = view _maxDuration protocolInfo.pDatum
    frDurationMinutes = durationToMinutes duration

  when (frDurationMinutes < minDurationMinutes) $ liftEffect $ throw ("Fundraising duration too short. It must be greater than " <> toString minDurationMinutes <> ".")
  when (frDurationMinutes > maxDurationMinutes) $ liftEffect $ throw ("Fundraising duration too long. It must be less than " <> toString maxDurationMinutes <> ".")

  now@(POSIXTime now') <- currentTime
  let deadline = addTimes now (minutesToPosixTime frDurationMinutes)
  serializedTitle <- liftContractM "Impossible to serialize a title" $ byteArrayFromAscii (take titleLength title)

  let
    initialFrDatum = PFundraisingDatum
      { creatorPkh: creds.ownPkh
      , tokenOrigin: creds.nonCollateralORef
      , frTitle: serializedTitle
      , frAmount: targetAmount
      , frDeadline: deadline
      , frFee: view _protocolFee protocolInfo.pDatum
      , managerAddress: view _managerAddress protocolInfo.pDatum
      }

  let
    fundraising = Fundraising
      { protocol: protocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }
  networkId <- getNetworkId
  frValidatorHash <- getFundraisingValidatorHash fundraising
  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress networkId frValidatorHash

  let
    protocolRedeemerData = PFundriseConfig
      { scriptAddress: frAddress
      , verCurrencySymbol: verTokenCs
      , verTokenName: verTn
      , threadCurrencySymbol: nftCs
      , threadTokenName: nftTn
      , startedAt: now'
      }

    protocolRedeemer = Redeemer $ toData (PStartFundrise protocolRedeemerData)

  let
    nftValue = Value.singleton nftCs nftTn one
    verTokenValue = Value.singleton verTokenCs verTn one
    paymentToFr = minAdaValue <> minAdaValue <> nftValue <> verTokenValue

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput creds.nonCollateralORef
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft nftTn)
          nftValue
        <> Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          verTokenPolicyHash
          (Redeemer $ toData $ PMintVerToken verTn)
          verTn
          one
          protocolInfo.references.verTokenInput
        <> Constraints.mustSpendScriptOutputUsingScriptRef
          (fst protocolInfo.pUtxo)
          protocolRedeemer
          protocolInfo.references.pRefScriptInput
        <> Constraints.mustPayToScriptAddress
          protocolInfo.pValidatorHash
          (ScriptCredential protocolInfo.pValidatorHash)
          (Datum $ toData protocolInfo.pDatum)
          Constraints.DatumInline
          protocolInfo.pValue
        <> Constraints.mustPayToScriptAddress
          frValidatorHash
          (ScriptCredential frValidatorHash)
          (Datum $ toData initialFrDatum)
          Constraints.DatumInline
          paymentToFr
        <> Constraints.mustBeSignedBy creds.ownPkh
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.pScriptRef)
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.verTokenRef)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy nftMp
        <> Lookups.unspentOutputs creds.ownUtxos
        <> Lookups.unspentOutputs protocolInfo.pUtxos

  completeTx lookups constraints ownCreds

  logInfo' "Fundraising created successfully"

  bech32Address <- addressToBech32 frAddress
  logInfo' $ "Current fundraising address: " <> show bech32Address

  creatorPkh <- pkhToBech32M creds.ownPkh

  pure $ FundraisingInfo
    { creator: Just creatorPkh
    , title: title
    , goal: targetAmount
    , raisedAmt: fromInt 0
    , deadline: deadline
    , threadTokenCurrency: currencySymbolToString nftCs
    , threadTokenName: fundraisingTokenNameString
    , isCompleted: false
    }
