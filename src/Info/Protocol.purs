module Info.Protocol where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Shared.TestnetConfig (mkTestnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash)
import Protocol.UserData (ProtocolConfigParams, mapFromProtocolDatum)
import Shared.Helpers (UtxoTuple, extractDatumFromUTxO, getUtxoByNFT)

runGetProtocolInfo :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
runGetProtocolInfo onComplete onError protocol = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig (protocolInfoContract protocol)
  where
  handler :: Either Error ProtocolConfigParams -> Effect Unit
  handler (Right protocolConfigParams) = onComplete protocolConfigParams
  handler (Left error) = onError $ message error

protocolInfoContract :: Protocol -> Contract ProtocolConfigParams
protocolInfoContract protocol = do
  logInfo' "Running get protocol info"
  protocolValidatorHash <- getProtocolValidatorHash protocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos

  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  pure $ mapFromProtocolDatum currentDatum

getProtocolUtxo :: Protocol -> UtxoMap -> Contract UtxoTuple
getProtocolUtxo protocol utxos =
  let
    p = unwrap protocol
  in
    getUtxoByNFT "Protocol" (Tuple (_.protocolCurrency p) (_.protocolTokenName p)) utxos
