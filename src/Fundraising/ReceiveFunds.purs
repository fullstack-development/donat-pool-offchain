module Fundraising.ReceiveFunds
  ( runReceiveFunds
  ) where

import Contract.Prelude

import Contract.Address (ownStakePubKeysHashes, AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes, validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.CurrencySymbol (adaSymbol)
import Ctl.Internal.Types.Interval (mkFiniteInterval)
import Ctl.Internal.Types.TokenName (adaToken)
import Data.Array (head) as Array
import Data.BigInt (BigInt, fromInt)
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import Fundraising.UserData (FundraisingData(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import Shared.Helpers (mkCurrencySymbol, mkBigIntRational, extractDatumFromUTxO, extractValueFromUTxO, getNonCollateralUtxo, getUtxoByNFT, roundBigIntRatio, checkTokenInUTxO)
import Shared.MinAda (minAda)
import Shared.RunContract (runContractWithUnitResult)

runReceiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> FundraisingData -> Effect Unit
runReceiveFunds onComplete onError fundraisingData = 
  runContractWithUnitResult onComplete onError $ contract fundraisingData

contract :: FundraisingData -> Contract () Unit
contract (FundraisingData fundraisingData) = do
  -- TODO: use mustPayToPubKeyAddress
  logInfo' "Running receive funds"

  let protocol = fundraisingData.protocol
  let threadTokenCurrency = fundraisingData.frThreadTokenCurrency
  let threadTokenName = fundraisingData.frThreadTokenName
  _ /\ verTokenCurrency <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTokenName <- VerToken.verTokenName
  let managerPkh = unwrap >>> _.managerPkh $ protocol
  let
    fundraising = Fundraising
      { protocol: protocol
      , verTokenCurrency: verTokenCurrency
      , verTokenName: verTokenName
      }

  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising
  frAddress <- liftContractM "Impossible to get Fundrising script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress

  -- TODO: refactor checks for frUtxo (similar check in donate endpoint)
  frUtxo <- getUtxoByNFT "Fundraising" (threadTokenCurrency /\ threadTokenName) frUtxos
  let isVerTokenInUtxo = checkTokenInUTxO (verTokenCurrency /\ verTokenName) frUtxo
  unless isVerTokenInUtxo $ throw >>> liftEffect $ "verToken is not in fundraising utxo"
  (PFundraisingDatum currentDatum) <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  let currentFunds = extractValueFromUTxO frUtxo
  logInfo' $ "Current funds: " <> show currentFunds

  now <- currentTime
  let donatedAmount = Value.valueOf currentFunds adaSymbol adaToken - minAda
  when (now <= currentDatum.frDeadline && donatedAmount /= currentDatum.frAmount) $ liftEffect $ throw "Can't receive funds while fundraising is in process"

  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get fundrising owner PaymentPubkeyHash" $ Array.head ownHashes
  when (ownPkh /= currentDatum.creatorPkh) $ liftEffect $ throw "Only fundraising creator can receive funds"
  mbOwnSkh <- join <<< Array.head <$> ownStakePubKeysHashes
  ownSkh <- liftContractM "Failed to get fundrising owner SKH" mbOwnSkh
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddress <- liftedM "Failed to get fundrising owner address" $ Array.head <$> getWalletAddresses
  ownUtxo <- utxosAt ownAddress >>= getNonCollateralUtxo

  let receiveFundsRedeemer = toData >>> Redeemer $ PReceiveFunds threadTokenCurrency threadTokenName
  let validateInConstraint = if (donatedAmount >= currentDatum.frAmount)
      then mempty
      else Constraints.mustValidateIn $ mkFiniteInterval currentDatum.frDeadline now
      
  let verTokenToBurnValue = Value.singleton verTokenCurrency verTokenName (fromInt (-1))
  let threadTokenToBurnValue = Value.singleton threadTokenCurrency threadTokenName (fromInt (-1))
  threadTokenMintingPolicy <- NFT.mintingPolicy currentDatum.tokenOrigin
  verTokenMintingPolicy <- VerToken.mintingPolicy protocol
  feeByFundrising <- calcFee currentDatum.frFee donatedAmount
  let amountToReceiver = Value.lovelaceValueOf $ (Value.valueOf currentFunds adaSymbol adaToken - feeByFundrising)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst frUtxo) receiveFundsRedeemer
        <> Constraints.mustBeSignedBy currentDatum.creatorPkh
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft threadTokenName)
          threadTokenToBurnValue
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft verTokenName)
          verTokenToBurnValue
        <> Constraints.mustPayToPubKeyAddress ownPkh ownSkh amountToReceiver
        <> Constraints.mustPayToPubKey managerPkh (Value.lovelaceValueOf feeByFundrising)
        <> validateInConstraint
  
  let
    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy threadTokenMintingPolicy
        <> Lookups.mintingPolicy verTokenMintingPolicy
        <> Lookups.validator frValidator
        <> Lookups.unspentOutputs frUtxos
        <> Lookups.unspentOutputs ownUtxo

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  let
    addressWithNetworkTag =
      AddressWithNetworkTag
        { address: ownAddress
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
