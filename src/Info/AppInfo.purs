module Info.AppInfo where

import Contract.Prelude
import Info.UserData (AppInfo(..), UserInfo(..))
import Contract.Address (addressWithNetworkTagToBech32, getNetworkId, validatorHashBaseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import Fundraising.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash)
import Protocol.UserData (getConfigFromProtocolDatum)
import Shared.Helpers (UtxoTuple, extractDatumFromUTxO, getUtxoByNFT)
import Shared.TestnetConfig (mkTestnetNamiConfig)

runGetAppInfo :: (AppInfo -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
runGetAppInfo onComplete onError protocol = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig (appInfoContract protocol)
  where
  handler :: Either Error AppInfo -> Effect Unit
  handler (Right appInfo) = onComplete appInfo
  handler (Left error) = onError $ message error

appInfoContract :: Protocol -> Contract AppInfo
appInfoContract protocol = do
  logInfo' "Running get protocol info"
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
