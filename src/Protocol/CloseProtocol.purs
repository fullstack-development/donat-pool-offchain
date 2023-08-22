module Protocol.CloseProtocol where

import Contract.Prelude

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
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
import Shared.Tx (completeTx)

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
  ownCreds@(OwnCredentials creds) <- getOwnCreds
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
        protocolInfo.references.pRefScriptInput
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PBurnNft protocolTokenName)
          nftToBurnValue
        <> Constraints.mustPayToPubKeyAddress
          creds.ownPkh
          creds.ownSkh
          (Value.lovelaceValueOf (fromInt 2000000))
        <> Constraints.mustBeSignedBy creds.ownPkh
        <> Constraints.mustReferenceOutput (fst protocolInfo.references.pScriptRef)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs protocolInfo.pUtxos

  completeTx lookups constraints ownCreds

  logInfo' "closeProtocol finished successfully"
