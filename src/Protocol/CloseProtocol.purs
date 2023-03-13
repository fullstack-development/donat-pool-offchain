module Protocol.CloseProtocol where

import Contract.Prelude

import Contract.Address (AddressWithNetworkTag(..), validatorHashBaseAddress, addressToBech32)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array (head) as Array
import Data.BigInt (fromInt)
import Data.Map (toUnfoldable) as Map
import Effect.Exception (throw)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.Redeemer (PProtocolRedeemer(PCloseProtocol))
import Shared.Helpers as Helpers
import Shared.RunContract (runContractWithUnitResult)
import Fundraising.OwnCredentials (OwnCredentials(..), getOwnCreds)

runCloseProtocolTest :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
runCloseProtocolTest onComplete onError protocol = runContractWithUnitResult onComplete onError $ contract protocol

contract :: Protocol -> Contract () Unit
contract protocol@(Protocol { protocolCurrency, protocolTokenName }) = do
  logInfo' "Running closeProtocol"
  logInfo' $ "Current protocol: " <> show protocol
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  bech32Address <- addressToBech32 protocolAddress
  logInfo' $ "Current protocol address: " <> show bech32Address

  protocolUTxOs <- utxosAt protocolAddress
  logInfo' $ "Protocol UTxOs: " <> show protocolUTxOs
  let protocolNft = Tuple protocolCurrency protocolTokenName
  desiredTxOut <-
    liftContractM "Protocol UTxO with current TrheadToken not found"
      (Array.head (Helpers.filterByToken protocolNft $ Map.toUnfoldable protocolUTxOs))
  logInfo' $ "Filtered protocol UTxO: " <> show desiredTxOut
  PProtocolDatum protocolDatum <- liftContractM "Impossible to get Protocol Datum" $ Helpers.extractDatumFromUTxO desiredTxOut
  logInfo' $ "Protocol UTxO Datum: " <> show protocolDatum
  let managerPkh = protocolDatum.managerPkh
  (OwnCredentials creds) <- getOwnCreds
  when (managerPkh /= creds.ownPkh) $ liftEffect $ throw "current user doesn't have permissions to close protocol"

  let nftOref = protocolDatum.tokenOriginRef
  mp <- NFT.mintingPolicy nftOref
  protocolValidator <- protocolValidatorScript protocol

  let
    protocolRedeemer = Redeemer $ toData PCloseProtocol
    nftToBurnValue = Value.singleton protocolCurrency protocolTokenName (fromInt (-1))

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput
        (fst desiredTxOut)
        protocolRedeemer
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft protocolTokenName)
          nftToBurnValue
        <> Constraints.mustPayToPubKeyAddress
          creds.ownPkh
          creds.ownSkh
          (Value.lovelaceValueOf (fromInt 2000000))

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs protocolUTxOs
        <> Lookups.unspentOutputs creds.ownUtxo
        <> Lookups.validator protocolValidator

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

  logInfo' "closeProtocol finished successfully"
