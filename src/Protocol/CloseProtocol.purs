module Protocol.CloseProtocol where

import Contract.Prelude

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt (fromInt)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(PCloseProtocol))
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds, getPkhSkhFromAddress)

runCloseProtocol :: Effect Unit
runCloseProtocol = do
  protocolConfig <- readProtocolConfig
  let protocolData = mapToProtocolData protocolConfig
  launchAff_ $ runContract testnetKeyWalletConfig (contract protocolData)

contract :: ProtocolData -> Contract Unit
contract protocolData = do
  logInfo' "Running closeProtocol"
  protocol@(Protocol { protocolCurrency, protocolTokenName }) <- dataToProtocol protocolData
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  managerPkh /\ _ <- getPkhSkhFromAddress (unwrap protocolInfo.pDatum).managerAddress
  (OwnCredentials creds) <- getOwnCreds
  when (managerPkh /= creds.ownPkh) $ liftEffect $ throw "current user doesn't have permissions to close protocol"
  let nftOref = (unwrap protocolInfo.pDatum).tokenOriginRef
  mp <- NFT.mintingPolicy nftOref
  let
    protocolRedeemer = Redeemer $ toData PCloseProtocol
    nftToBurnValue = Value.singleton protocolCurrency protocolTokenName (fromInt (-1))

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst protocolInfo.pUtxo)
        protocolRedeemer
        protocolInfo.pRefScriptInput
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft protocolTokenName)
          nftToBurnValue
        <> Constraints.mustPayToPubKeyAddress
          creds.ownPkh
          creds.ownSkh
          (Value.lovelaceValueOf (fromInt 2000000))
        <> Constraints.mustBeSignedBy creds.ownPkh
        <> Constraints.mustReferenceOutput (fst protocolInfo.pScriptRef)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs protocolInfo.pUtxos

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <- liftedM "Failed to get own address with Network Tag" $ Array.head <$> getWalletAddressesWithNetworkTag
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId

  logInfo' "closeProtocol finished successfully"
