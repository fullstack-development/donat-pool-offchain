module Info.AppInfo where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Info.UserData (AppInfo(..))
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash)
import Protocol.UserData (ProtocolData, dataToProtocol, getConfigFromProtocolDatum)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (getOwnUserInfo, getPkhSkhFromAddress)
import Shared.RunContract (runContractWithResult)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, getUtxoByNFT)

runGetAppInfo :: (AppInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
runGetAppInfo onComplete onError protocolData networkParams =
  runContractWithResult onComplete onError networkParams (appInfoContract protocolData)

appInfoContract :: ProtocolData -> Contract AppInfo
appInfoContract protocolData = do
  logInfo' "Running get protocol info"
  protocol <- dataToProtocol protocolData
  protocolValidatorHash <- getProtocolValidatorHash protocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  utxos <- utxosAt protocolAddress
  protocolUtxo <- getProtocolUtxo protocol utxos
  protocolDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current datum: " <> show protocolDatum
  managerPkh /\ _ <- getPkhSkhFromAddress (unwrap protocolDatum).managerAddress
  userInfo <- getOwnUserInfo managerPkh

  let
    appInfo = AppInfo
      { protocolConfig: getConfigFromProtocolDatum protocolDatum
      , userInfo: userInfo
      }

  pure appInfo

getProtocolUtxo :: Protocol -> UtxoMap -> Contract UtxoTuple
getProtocolUtxo protocol utxos =
  let
    p = unwrap protocol
  in
    getUtxoByNFT "Protocol" (Tuple (_.protocolCurrency p) (_.protocolTokenName p)) utxos
