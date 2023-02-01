module Info.Protocol where

import Contract.Prelude
import Contract.Monad (ConfigParams, Contract, liftContractM, runContract)
import Effect.Aff (launchAff, Fiber)
import Protocol.Models (Protocol)
import Contract.Address (validatorHashBaseAddress)
import Contract.Config (NetworkId(..), testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Protocol.Datum (PProtocolDatum)
import Protocol.ProtocolScript (getProtocolValidatorHash)
import Shared.Helpers (UtxoTuple, extractDatumFromUTxO, getUtxoByThreadToken)

runGetProtocolInfo :: Protocol -> Effect (Fiber PProtocolDatum)
runGetProtocolInfo protocol = (getProtocolInfo protocol) testnetNamiConfig

getProtocolInfo :: Protocol -> ConfigParams () -> Effect (Fiber PProtocolDatum)
getProtocolInfo protocol baseConfig = launchAff $ do
  runContract baseConfig (protocolInfoContract protocol)

protocolInfoContract :: Protocol -> Contract () PProtocolDatum
protocolInfoContract protocol = do
  logInfo' "Running get protocol info"
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress TestnetId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos

  currentDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current datum: " <> show currentDatum
  pure currentDatum

getProtocolUtxo :: Protocol -> UtxoMap -> Contract () UtxoTuple
getProtocolUtxo protocol utxos =
  let
    p = unwrap protocol
  in
    getUtxoByThreadToken (Tuple (_.protocolCurrency p) (_.protocolTokenName p)) utxos
