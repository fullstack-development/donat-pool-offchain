module Fundraising.ReceiveFunds
  ( runReceiveFunds
  ) where

import Contract.Prelude

import Data.Ratio ((%))
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Contract.Address (AddressWithNetworkTag(..), getWalletAddresses, ownPaymentPubKeysHashes, validatorHashBaseAddress)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Config (testnetNamiConfig, NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.CurrencySymbol (adaSymbol)
import Ctl.Internal.Types.Interval (Interval(..))
import Ctl.Internal.Types.TokenName (adaToken)
import Data.Array (head) as Array
import Data.BigInt (BigInt, fromInt)
import Effect.Exception (throw)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.Redeemer (PFundraisingRedeemer(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.Models (Protocol(..))
import Shared.Helpers
  ( mkBigIntRational
  , extractDatumFromUTxO
  , extractValueFromUTxO
  , getNonCollateralUtxo
  , getUtxoByNFT
  , bigIntRatioToNumber
  )
import Shared.MinAda (minAda, minAdaInt)
import Data.Int (round)

runReceiveFunds :: Protocol -> Fundraising -> Effect Unit
runReceiveFunds protocol fundrising = launchAff_ do
  runContract testnetNamiConfig (contract protocol fundrising)

contract :: Protocol -> Fundraising -> Contract () Unit
contract protocol'@(Protocol protocol) fundrising'@(Fundraising fundrising) = do
  logInfo' "Running receive funds"
  frValidator <- fundraisingValidatorScript fundrising'
  frValidatorHash <- getFundraisingValidatorHash fundrising'
  frAddress <- liftContractM "Impossible to get Fundrising script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress
  frUtxo <- getUtxoByNFT "Fundraising" (fundrising.verTokenCurrency /\ fundrising.verTokenName) frUtxos

  (PFundraisingDatum currentDatum) <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  let currentFunds = extractValueFromUTxO frUtxo
  logInfo' $ "Current funds: " <> show currentFunds

  now <- currentTime
  let donatedAmount = Value.valueOf currentFunds adaSymbol adaToken - minAda
  when (now <= currentDatum.frDeadline && donatedAmount /= currentDatum.frAmount) $ liftEffect $ throw "Can't receive funds while fundrising is in process"

  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  when (ownPkh /= currentDatum.creatorPkh) $ liftEffect $ throw "Only fundrising creator can receive funds"

  ownAddress <- liftedM "Failed to get donator address" $ Array.head <$> getWalletAddresses
  ownUtxo <- utxosAt ownAddress >>= getNonCollateralUtxo

  let donateRedeemer = Redeemer $ toData PReceiveFunds
  let donationTimeRange = FiniteInterval now currentDatum.frDeadline
  let amountToReceiver = Value.lovelaceValueOf $ (Value.valueOf currentFunds adaSymbol adaToken - currentDatum.frFee)

  let verTokenToBurnValue = Value.singleton fundrising.verTokenCurrency fundrising.verTokenName (fromInt (-1))
  let threadTokenToBurnValue = Value.singleton fundrising.threadTokenCurrency fundrising.threadTokenName (fromInt (-1))
  threadTokenMintingPolicy <- NFT.mintingPolicy currentDatum.tokenOrigin
  verTokenMintingPolicy <- VerToken.mintingPolicy protocol'
  feeByFundrising <- calcFee currentDatum.frFee donatedAmount

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput (fst frUtxo) donateRedeemer
        <> Constraints.mustBeSignedBy currentDatum.creatorPkh
        <> Constraints.mustValidateIn donationTimeRange
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft fundrising.threadTokenName)
          threadTokenToBurnValue
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft fundrising.verTokenName)
          verTokenToBurnValue
        <> Constraints.mustPayToPubKey ownPkh amountToReceiver
        <> Constraints.mustPayToPubKey protocol.managerPkh (Value.lovelaceValueOf feeByFundrising)
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
  logInfo' "Donate finished successfully"

calcFee :: BigInt -> BigInt -> Contract () BigInt
calcFee feePercent funds' = do
  fee <- maybe rationalErr pure $ mkBigIntRational (feePercent /\ funds')
  let funds = funds' % fromInt 1
  let res = fee * funds
  let res' = bigIntRatioToNumber res
  -- TODO: Warning! This can be a source of errors: round :: Number -> Int
  -- But should be used roundToBigInt from Helpers
  -- can't import Number.round
  pure $ fromInt $ max (round res') minAdaInt
  where
  rationalErr = liftEffect $ throw "Can't make rational fee"