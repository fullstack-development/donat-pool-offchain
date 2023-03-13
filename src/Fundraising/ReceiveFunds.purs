module Fundraising.ReceiveFunds
  ( runReceiveFunds
  ) where

import Contract.Prelude

import Fundraising.FundrisingScriptInfo (FundrisingScriptInfo(..), getFundrisingScriptInfo, makeFundrising)
import Contract.Address (AddressWithNetworkTag(..), validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftContractM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  , mkTxUnspentOut
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.CurrencySymbol (adaSymbol)
import Ctl.Internal.Types.Interval (mkFiniteInterval)
import Ctl.Internal.Types.TokenName (adaToken)
import Data.BigInt (BigInt, fromInt)
import Data.Lens (view)
import Data.Map as Map
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.Models (Fundraising(..))
import Fundraising.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import Info.Protocol (getProtocolUtxo)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.Datum (_managerPkh)
import Protocol.ProtocolScript (protocolValidatorScript, getProtocolValidatorHash)
import Shared.Helpers (checkTokenInUTxO, mkBigIntRational, roundBigIntRatio, extractDatumFromUTxO, hasRefPlutusScript)
import Shared.MinAda (minAda)
import Shared.RunContract (runContractWithUnitResult)

runReceiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> FundraisingData -> Effect Unit
runReceiveFunds onComplete onError fundraisingData =
  runContractWithUnitResult onComplete onError $ contract fundraisingData

contract :: FundraisingData -> Contract () Unit
contract frData@(FundraisingData fundraisingData) = do
  -- TODO: use mustPayToPubKeyAddress for managerPkh (need stake key hash)
  logInfo' "Running receive funds"
  -- let managerPkh = unwrap >>> _.managerPkh $ protocol
  let protocol = fundraisingData.protocol
  protocolValidator <- protocolValidatorScript protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <- liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos
  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current protocol datum: " <> show currentDatum

  refValidatorInput /\ refValidatorOutput <-
    liftContractM "Could not find unspent output containing ref validator"
      $ find (hasRefPlutusScript $ unwrap protocolValidator) (Map.toUnfoldable utxos :: Array _)

  let managerPkh = view _managerPkh currentDatum

  let threadTokenCurrency = fundraisingData.frThreadTokenCurrency
  let threadTokenName = fundraisingData.frThreadTokenName
  fundraising@(Fundraising fr) <- makeFundrising frData
  (FundrisingScriptInfo frInfo) <- getFundrisingScriptInfo fundraising threadTokenCurrency threadTokenName
  let isVerTokenInUtxo = checkTokenInUTxO (fr.verTokenCurrency /\ fr.verTokenName) frInfo.frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  let
    currentFunds = frInfo.frValue
    (PFundraisingDatum currentDatum) = frInfo.frDatum

  now <- currentTime
  let donatedAmount = Value.valueOf currentFunds adaSymbol adaToken - minAda
  when (now <= currentDatum.frDeadline && donatedAmount /= currentDatum.frAmount) $ liftEffect $ throw "Can't receive funds while fundraising is in process"

  (OwnCredentials creds) <- getOwnCreds
  when (creds.ownPkh /= currentDatum.creatorPkh) $ liftEffect $ throw "Only fundraising creator can receive funds"

  let receiveFundsRedeemer = toData >>> Redeemer $ PReceiveFunds threadTokenCurrency threadTokenName
  let
    -- TODO: test interval
    validateInConstraint =
      if (donatedAmount >= currentDatum.frAmount) then mempty
      else Constraints.mustValidateIn $ mkFiniteInterval currentDatum.frDeadline now
  let verTokenToBurnValue = Value.singleton fr.verTokenCurrency fr.verTokenName (fromInt (-1))
  let threadTokenToBurnValue = Value.singleton threadTokenCurrency threadTokenName (fromInt (-1))
  threadTokenMintingPolicy <- NFT.mintingPolicy currentDatum.tokenOrigin
  verTokenMintingPolicy <- VerToken.mintingPolicy protocol
  feeByFundrising <- calcFee currentDatum.frFee donatedAmount
  let amountToReceiver = Value.lovelaceValueOf $ (Value.valueOf currentFunds adaSymbol adaToken - feeByFundrising)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst frInfo.frUtxo)
        receiveFundsRedeemer
        (Constraints.RefInput $ mkTxUnspentOut refValidatorInput refValidatorOutput)
        <> Constraints.mustBeSignedBy currentDatum.creatorPkh
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft threadTokenName)
          threadTokenToBurnValue
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft fr.verTokenName)
          verTokenToBurnValue
        <> Constraints.mustPayToPubKeyAddress creds.ownPkh creds.ownSkh amountToReceiver
        <> Constraints.mustPayToPubKey managerPkh (Value.lovelaceValueOf feeByFundrising)
        <> validateInConstraint

  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy threadTokenMintingPolicy
        <> Lookups.mintingPolicy verTokenMintingPolicy
        <> Lookups.validator frInfo.frValidator
        <> Lookups.unspentOutputs frInfo.frUtxos
        <> Lookups.unspentOutputs creds.ownUtxo

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    addressWithNetworkTag =
      AddressWithNetworkTag
        { address: creds.ownAddress
        , networkId: TestnetId
        }

    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' "Receive funds finished successfully"

calcFee :: BigInt -> BigInt -> Contract () BigInt
calcFee feePercent funds' = do
  fee <- maybe rationalErr pure $ mkBigIntRational (feePercent * funds' /\ fromInt 100)
  rounded <- maybe roundErr pure $ roundBigIntRatio fee
  let feeAmount = max rounded minAda
  logInfo' $ "FeeAmount: " <> show feeAmount
  pure feeAmount
  where
  rationalErr = liftEffect $ throw "Can't make rational fee"
  roundErr = liftEffect $ throw "Can't create BigInt after round"