module Info.UserRelatedFundraisings where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Data.Array as Array
import Ext.Seriaization.Key (pkhToBech32M)
import Info.AllFundraisings (getAllFundraisings)
import Info.UserData (FundraisingInfo, filterByPkh)
import Protocol.UserData (ProtocolData)
import Shared.NetworkData (NetworkParams)
import Shared.RunContract (runContractWithResult)

runGetUserRelatedFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> NetworkParams -> Effect Unit
runGetUserRelatedFundraisings onComplete onError protocolData networkParams = do
  runContractWithResult onComplete onError networkParams (getUserRelatedFundraisings protocolData)

getUserRelatedFundraisings :: ProtocolData -> Contract (Array FundraisingInfo)
getUserRelatedFundraisings protocolData = do
  allFrs <- getAllFundraisings protocolData
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  pkh <- pkhToBech32M ownPkh
  let userFrs = filterByPkh pkh allFrs
  logInfo' $ "Discovered items: " <> show userFrs
  pure userFrs
