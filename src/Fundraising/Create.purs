module Fundraising.Create where

import Contract.Prelude

import Contract.Address (getWalletAddresses, ownPaymentPubKeysHashes, AddressWithNetworkTag(..), addressToBech32, validatorHashBaseAddress)
import Contract.Config (ConfigParams, testnetNamiConfig, NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftedM, liftedE)
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , Datum(Datum)
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array (head) as Array
import Data.BigInt (fromInt)
import Data.Map (toUnfoldable) as Map
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.VerTokenMinting as VerToken
import MintingPolicy.VerTokenRedeemers (PVerTokenRedeemer(..))
import Shared.Helpers as Helpers
import Protocol.Models (Protocol, PFundriseConfig(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustSendChangeToAddress
  )
import Protocol.Datum
  ( _protocolFee
  , _minDuration
  , _maxDuration
  , _minAmount
  , _maxAmount
  )
import Contract.Credential (Credential(ScriptCredential))
import Fundraising.UserData (CreateFundraisingParams(..))
import Fundraising.FundraisingScript (fundraisingTokenName, fundraisingValidatorScript, getFundraisingValidatorHash)
import Info.Protocol (getProtocolUtxo)
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Data.Lens (view)
import Effect.Exception (throw)
import Contract.Chain (currentTime)
import Contract.Time (POSIXTime(..))
import Protocol.Redeemer (PProtocolRedeemer(..))
import Fundraising.Models (Fundraising(..))
import Fundraising.Datum (PFundraisingDatum(..))

contract :: Protocol -> CreateFundraisingParams -> Contract () Fundraising
contract givenProtocol (CreateFundraisingParams { description, amount, duration }) = do
  logInfo' "Running Create Fundraising contract"
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh

  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  logInfo' $ "Own address is: " <> show ownAddress
  ownUtxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show ownUtxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Helpers.filterNonCollateral $ Map.toUnfoldable ownUtxos))
  nftMp /\ nftCs <- Helpers.mkCurrencySymbol (NFT.mintingPolicy oref)
  nftTn <- fundraisingTokenName

  verTokenMp /\ verTokenCs <- Helpers.mkCurrencySymbol (VerToken.mintingPolicy givenProtocol)
  verTn <- VerToken.verTokenName

  protocolValidator <- protocolValidatorScript givenProtocol
  protocolValidatorHash <- getProtocolValidatorHash givenProtocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  protocolUtxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo givenProtocol protocolUtxos
  protocolDatum <- liftContractM "Impossible to get Protocol Datum" $ Helpers.extractDatumFromUTxO protocolUtxo

  let
    minAmount = view _minAmount protocolDatum
    maxAmount = view _maxAmount protocolDatum
    currentAmount = fromInt amount

  when (currentAmount < minAmount) $ liftEffect $ throw ("Fundraising amount too small. It must be greater than " <> show minAmount <> ".")
  when (currentAmount > maxAmount) $ liftEffect $ throw ("Fundraising amount too big. It must be less than " <> show maxAmount <> ".")

  let
    minDuration = view _minDuration protocolDatum
    maxDuration = view _maxDuration protocolDatum
    frDuration = fromInt duration

  when (frDuration < minDuration) $ liftEffect $ throw ("Fundraising duration too short. It must be greater than " <> show minDuration <> ".")
  when (frDuration > maxDuration) $ liftEffect $ throw ("Fundraising duration too long. It must be less than " <> show maxDuration <> ".")

  now@(POSIXTime now') <- currentTime
  let deadline = Helpers.addTimes now (Helpers.daysToPosixTime duration)

  desc <- liftContractM "Impossible to serialize description" $ hexToByteArray description

  let
    initialFrDatum = PFundraisingDatum
      { creatorPkh: ownPkh
      , tokenOrigin: oref
      , frDesc: desc
      , frAmount: currentAmount
      , frDeadline: deadline
      , frFee: view _protocolFee protocolDatum
      }

  let
    fundraising = Fundraising
      { protocol: givenProtocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }

  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising

  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress TestnetId frValidatorHash

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
    paymentToProtocol = Helpers.extractValueFromUTxO protocolUtxo
    paymentToFr = Value.lovelaceValueOf (fromInt 2000000) <> nftValue <> verTokenValue

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput oref
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft nftTn)
          nftValue
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintVerToken verTn)
          verTokenValue
        <> Constraints.mustSpendScriptOutput
          (fst protocolUtxo)
          protocolRedeemer
        <> Constraints.mustPayToScriptAddress
          protocolValidatorHash
          (ScriptCredential protocolValidatorHash)
          (Datum $ toData protocolDatum)
          Constraints.DatumInline
          paymentToProtocol
        <> Constraints.mustPayToScriptAddress
          frValidatorHash
          (ScriptCredential frValidatorHash)
          (Datum $ toData initialFrDatum)
          Constraints.DatumInline
          paymentToFr

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy nftMp
        <> Lookups.mintingPolicy verTokenMp
        <> Lookups.unspentOutputs ownUtxos
        <> Lookups.validator protocolValidator
        <> Lookups.validator frValidator

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

  logInfo' "Fundraising created successfully"

  logInfo' $ "Current fundraising: " <> show fundraising

  pure fundraising
