module Fundraising.ReceiveFunds
  ( runReceiveFunds
  , contract
  ) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicyHash, mintingPolicyHash)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.CurrencySymbol (adaSymbol)
import Ctl.Internal.Types.Interval (from)
import Ctl.Internal.Types.TokenName (adaToken)
import Data.BigInt (BigInt, fromInt)
import Effect.Exception (throw)
import Ext.Contract.Time (posixToTimeStamp, roundToSecond)
import Ext.Contract.Value (mkCurrencySymbolFromString, runMkTokenName)
import FeePool.Constraints as FeePool
import FeePoolInfo.Constraints as FeePoolInfo
import Fundraising.Calculations (calcFee)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScriptInfo (FundraisingScriptInfo(..), getFundraisingScriptInfo, makeFundraising)
import Fundraising.Models (Fundraising(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import MintingPolicy.VerTokenRedeemers (PVerTokenRedeemer(..))
import Protocol.Models (Protocol)
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (minAda, twoMinAda)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds, getPkhSkhFromAddress)
import Shared.RunContract (runContractWithResult)
import Shared.Tx (completeTx)
import Shared.Utxo (checkTokenInUTxO)

runReceiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> FundraisingData -> Effect Unit
runReceiveFunds onComplete onError pData networkParams fundraisingData =
  runContractWithResult onComplete onError networkParams $ contract pData fundraisingData

contract :: ProtocolData -> FundraisingData -> Contract Unit
contract pData (FundraisingData fundraisingData) = do
  logInfo' "Running receive funds"
  protocol <- dataToProtocol pData
  threadTokenCurrency <- mkCurrencySymbolFromString fundraisingData.frThreadTokenCurrency
  threadTokenName <- runMkTokenName fundraisingData.frThreadTokenName
  fundraising@(Fundraising fr) <- makeFundraising pData
  frInfo'@(FundraisingScriptInfo frInfo) <- getFundraisingScriptInfo fundraising threadTokenCurrency threadTokenName
  let isVerTokenInUtxo = checkTokenInUTxO (fr.verTokenCurrency /\ fr.verTokenName) frInfo.frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  let
    currentFunds = frInfo.frValue
    (PFundraisingDatum currentDatum) = frInfo.frDatum
  managerPkh /\ managerSkh <- getPkhSkhFromAddress currentDatum.managerAddress
  now' <- currentTime
  let now = roundToSecond now'
  let donatedAmount = Value.valueOf currentFunds adaSymbol adaToken - twoMinAda
  when (now <= currentDatum.frDeadline && donatedAmount < currentDatum.frAmount)
    $ liftEffect
    $ throw "Can't receive funds while fundraising is in progress"

  ownCreds@(OwnCredentials creds) <- getOwnCreds
  when (creds.ownPkh /= currentDatum.creatorPkh) $ liftEffect $ throw "Only fundraising creator can receive funds"
  threadTokenMintingPolicy <- NFT.mintingPolicy currentDatum.tokenOrigin
  feeAmount <- liftContractM "Can't create BigInt after round" $ calcFee currentDatum.frFee donatedAmount
  let timestamp = posixToTimeStamp now
  feePoolEpoch <- FeePool.getFeePoolEpoch protocol
  let isCurrentEpoch = timestamp.epoch == feePoolEpoch
  let
    amountToReceiver = Value.lovelaceValueOf $ (Value.valueOf currentFunds adaSymbol adaToken - feeAmount)
    -- we need the fee amount greater than two minAda: one minAda goes to manager's wallet, one minAda may go to 
    -- the new FeePoolInfo UTxO (if it's a new epoch) and the rest goes to FeePool
    feeTooSmallToUseFeePool = feeAmount <= twoMinAda
    amountToManager = if feeTooSmallToUseFeePool then feeAmount else minAda
  frConstraints /\ frLookups <- mkFundraisingConstraints protocol feeAmount threadTokenCurrency threadTokenName isCurrentEpoch frInfo'
  feePoolConstraints /\ feePoolLookups <- FeePool.mkReceiveFundsConstraints protocol now feeAmount
  logInfo' $ "FeePool constraints: " <> show feePoolConstraints
  logInfo' $ "FeePool lookups: " <> show feePoolLookups
  feePoolInfoConstraints /\ feePoolInfoLookups <- FeePoolInfo.mkReceiveFundsConstraints protocol now feeAmount
  logInfo' $ "FeePoolInfo constraints: " <> show feePoolInfoConstraints
  logInfo' $ "FeePoolInfo lookups: " <> show feePoolInfoLookups
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      frConstraints
        <> feePoolConstraints
        <> feePoolInfoConstraints
        <> Constraints.mustBeSignedBy currentDatum.creatorPkh
        <> Constraints.mustPayToPubKeyAddress creds.ownPkh creds.ownSkh amountToReceiver
        <> Constraints.mustPayToPubKeyAddress managerPkh managerSkh (Value.lovelaceValueOf amountToManager)
        <> Constraints.mustValidateIn (from now')

  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy threadTokenMintingPolicy
        <> frLookups
        <> feePoolLookups
        <> feePoolInfoLookups

  completeTx lookups constraints ownCreds

  logInfo' "Receive funds finished successfully"

mkFundraisingConstraints
  :: Protocol
  -> BigInt
  -> Value.CurrencySymbol
  -> Value.TokenName
  -> Boolean
  -> FundraisingScriptInfo
  -> Contract (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mkFundraisingConstraints protocol feeAmount threadTokenCurrency threadTokenName isCurrentEpoch (FundraisingScriptInfo frInfo) = do
  verTokenMintingPolicy <- VerToken.mintingPolicy protocol
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  verTokenName <- VerToken.verTokenName
  let
    verTokenPolicyHash :: MintingPolicyHash
    verTokenPolicyHash = mintingPolicyHash verTokenMintingPolicy

    threadTokenToBurnValue = Value.singleton threadTokenCurrency threadTokenName (fromInt (-1))
    receiveFundsRedeemer = Redeemer <<< toData $ mkReceiveFundsRedeemer feeAmount threadTokenCurrency threadTokenName isCurrentEpoch
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst frInfo.frUtxo)
        receiveFundsRedeemer
        frInfo.frRefScriptInput
        <> Constraints.mustReferenceOutput (fst frInfo.frScriptRef)
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft threadTokenName)
          threadTokenToBurnValue
        <> Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          verTokenPolicyHash
          (Redeemer $ toData $ PBurnVerToken verTokenName)
          verTokenName
          (fromInt (-1))
          protocolInfo.references.verTokenInput
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.verTokenRef)

    lookups = Lookups.unspentOutputs frInfo.frUtxos <> Lookups.unspentOutputs protocolInfo.pUtxos
  pure (constraints /\ lookups)

mkReceiveFundsRedeemer :: BigInt -> Value.CurrencySymbol -> Value.TokenName -> Boolean -> PFundraisingRedeemer
mkReceiveFundsRedeemer feeAmt threadTokenCurrency threadTokenName isCurrentEpoch =
  if feeAmt <= twoMinAda then PReceiveFundsWithoutFee threadTokenCurrency threadTokenName
  else if isCurrentEpoch then PReceiveFundsCurrentEpoch threadTokenCurrency threadTokenName
  else PReceiveFundsNewEpoch threadTokenCurrency threadTokenName
  where
  twoMinAda = minAda + minAda
