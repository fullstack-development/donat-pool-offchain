module Fundraising.ReceiveFunds
  ( runReceiveFunds
  , contract
  ) where

import Contract.Prelude

import Fundraising.FundrisingScriptInfo (FundrisingScriptInfo(..), getFundrisingScriptInfo, makeFundrising)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftContractM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.CurrencySymbol (adaSymbol)
import Ctl.Internal.Types.Interval (from)
import Ctl.Internal.Types.TokenName (adaToken)
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Fundraising.Calculations (calcFee)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.Models (Fundraising(..))
import Fundraising.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import Shared.Helpers (checkTokenInUTxO)
import Shared.MinAda (minAda)
import Shared.RunContract (runContractWithUnitResult)

runReceiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> FundraisingData -> Effect Unit
runReceiveFunds onComplete onError fundraisingData =
  runContractWithUnitResult onComplete onError $ contract fundraisingData

contract :: FundraisingData -> Contract Unit
contract frData@(FundraisingData fundraisingData) = do
  -- TODO: use mustPayToPubKeyAddress for managerPkh (need stake key hash)
  logInfo' "Running receive funds"
  let protocol = fundraisingData.protocol
  let threadTokenCurrency = fundraisingData.frThreadTokenCurrency
  let threadTokenName = fundraisingData.frThreadTokenName
  fundraising@(Fundraising fr) <- makeFundrising frData
  (FundrisingScriptInfo frInfo) <- getFundrisingScriptInfo fundraising threadTokenCurrency threadTokenName
  let isVerTokenInUtxo = checkTokenInUTxO (fr.verTokenCurrency /\ fr.verTokenName) frInfo.frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  let
    currentFunds = frInfo.frValue
    (PFundraisingDatum currentDatum) = frInfo.frDatum
    managerPkh = currentDatum.managerPkh
  now <- currentTime
  let donatedAmount = Value.valueOf currentFunds adaSymbol adaToken - minAda - minAda
  when (now <= currentDatum.frDeadline && donatedAmount /= currentDatum.frAmount) $ liftEffect $ throw "Can't receive funds while fundraising is in process"

  (OwnCredentials creds) <- getOwnCreds
  when (creds.ownPkh /= currentDatum.creatorPkh) $ liftEffect $ throw "Only fundraising creator can receive funds"

  let receiveFundsRedeemer = toData >>> Redeemer $ PReceiveFunds threadTokenCurrency threadTokenName

  let verTokenToBurnValue = Value.singleton fr.verTokenCurrency fr.verTokenName (fromInt (-1))
  let threadTokenToBurnValue = Value.singleton threadTokenCurrency threadTokenName (fromInt (-1))
  threadTokenMintingPolicy <- NFT.mintingPolicy currentDatum.tokenOrigin
  verTokenMintingPolicy <- VerToken.mintingPolicy protocol
  feeByFundrising <- liftContractM "Can't create BigInt after round" $ calcFee currentDatum.frFee donatedAmount
  let amountToReceiver = Value.lovelaceValueOf $ (Value.valueOf currentFunds adaSymbol adaToken - feeByFundrising)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput
        (fst frInfo.frUtxo)
        receiveFundsRedeemer
        <> Constraints.mustBeSignedBy currentDatum.creatorPkh
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft threadTokenName)
          threadTokenToBurnValue
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft fr.verTokenName)
          verTokenToBurnValue
        <> Constraints.mustPayToPubKeyAddress creds.ownPkh creds.ownSkh amountToReceiver
        <> Constraints.mustPayToPubKey managerPkh (Value.lovelaceValueOf feeByFundrising)
        <> Constraints.mustValidateIn (from now)

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
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress creds.ownAddressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId
  logInfo' "Receive funds finished successfully"
