module Info.AppInfo where

import Contract.Prelude

import Contract.Address (addressWithNetworkTagToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import Fundraising.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Info.UserData (AppInfo(..), UserInfo(..))
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash)
import Protocol.UserData (ProtocolData, dataToProtocol, getConfigFromProtocolDatum)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, getUtxoByNFT)
import Shared.TestnetConfig (mkTestnetNamiConfig)

runGetAppInfo :: (AppInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
runGetAppInfo onComplete onError protocolData = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig (appInfoContract protocolData)
  where
  handler :: Either Error AppInfo -> Effect Unit
  handler (Right appInfo) = onComplete appInfo
  handler (Left error) = onError $ message error

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
  (OwnCredentials creds) <- getOwnCreds

  protocolDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Current datum: " <> show protocolDatum
  let managerPkh = unwrap >>> _.managerPkh $ protocolDatum
  let
    userInfo = UserInfo
      { address: addressWithNetworkTagToBech32 creds.ownAddressWithNetworkTag
      , isManager: creds.ownPkh == managerPkh
      }
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
