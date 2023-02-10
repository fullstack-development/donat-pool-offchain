module Info.Protocol where

import Contract.Prelude
import Contract.Monad (Contract, liftContractM, runContract)
import Effect.Aff (runAff_)
import Protocol.Models (Protocol)
import Contract.Address (validatorHashBaseAddress)
import Contract.Config (NetworkId(..), testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Protocol.ProtocolScript (getProtocolValidatorHash)
import Shared.Helpers (UtxoTuple, extractDatumFromUTxO, getUtxoByThreadToken)
import Protocol.UserData (ProtocolConfigParams, mapFromProtocolDatum)
import Effect.Exception (Error, message)

runGetProtocolInfo :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
runGetProtocolInfo onComplete onError protocol = runAff_ handler $ do
  runContract testnetNamiConfig (protocolInfoContract protocol)
  where
  handler :: Either Error ProtocolConfigParams -> Effect Unit
  handler (Right protocolConfigParams) = onComplete protocolConfigParams
  handler (Left error) = onError $ message error

protocolInfoContract :: Protocol -> Contract () ProtocolConfigParams
protocolInfoContract protocol = do
  logInfo' "Running get protocol info"
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos

  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  pure $ mapFromProtocolDatum currentDatum

getProtocolUtxo :: Protocol -> UtxoMap -> Contract () UtxoTuple
getProtocolUtxo protocol utxos =
  let
    p = unwrap protocol
  in
    getUtxoByThreadToken (Tuple (_.protocolCurrency p) (_.protocolTokenName p)) utxos
