module Info.UserRelatedFundraisings where

import Contract.Prelude

import Contract.Address (addressToBech32)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Info.AllFundraisings (getAllFundraisings)
import Info.UserData (FundraisingInfo, filterByCreatorAddress)
import Protocol.UserData (ProtocolData)
import Shared.NetworkData (NetworkParams)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.RunContract (runContractWithResult)

runGetUserRelatedFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
runGetUserRelatedFundraisings onComplete onError protocolData networkParams = do
  runContractWithResult onComplete onError networkParams (getUserRelatedFundraisings protocolData)

getUserRelatedFundraisings :: ProtocolData -> Contract (Array FundraisingInfo)
getUserRelatedFundraisings protocolData = do
  allFrs <- getAllFundraisings protocolData
  OwnCredentials creds <- getOwnCreds
  address <- addressToBech32 creds.ownAddress
  logInfo' $ "Own Address is: " <> show address
  let userFrs = filterByCreatorAddress address allFrs
  logInfo' $ "Discovered items: " <> show userFrs
  pure userFrs
