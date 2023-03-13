module Protocol.CloseProtocol where

import Contract.Prelude

import Contract.Address (AddressWithNetworkTag(..))
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Config (NetworkId(TestnetId))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Models (Protocol(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(PCloseProtocol))
import Shared.RunContract (runContractWithUnitResult)
import Fundraising.OwnCredentials (OwnCredentials(..), getOwnCreds)

runCloseProtocolTest :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
runCloseProtocolTest onComplete onError protocol = runContractWithUnitResult onComplete onError $ contract protocol

contract :: Protocol -> Contract () Unit
contract protocol@(Protocol { protocolCurrency, protocolTokenName }) = do
  logInfo' "Running closeProtocol"
  (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  let managerPkh = (unwrap protocolInfo.pDatum).managerPkh
  (OwnCredentials creds) <- getOwnCreds
  when (managerPkh /= creds.ownPkh) $ liftEffect $ throw "current user doesn't have permissions to close protocol"
  let nftOref = (unwrap protocolInfo.pDatum).tokenOriginRef
  mp <- NFT.mintingPolicy nftOref
  let
    protocolRedeemer = Redeemer $ toData PCloseProtocol
    nftToBurnValue = Value.singleton protocolCurrency protocolTokenName (fromInt (-1))

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput
        (fst protocolInfo.pUtxo)
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
        <> Lookups.unspentOutputs protocolInfo.pUtxos
        <> Lookups.unspentOutputs creds.ownUtxo
        <> Lookups.validator protocolInfo.pValidator

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
