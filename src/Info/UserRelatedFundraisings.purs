module Info.UserRelatedFundraisings where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeysHashes)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Data.Array as Array
import Effect.Aff (runAff_)
import Effect.Exception (Error, message)
import Info.AllFundraisings (getAllFundraisings)
import Info.UserData (FundraisingInfo, filterByPkh)
import Protocol.UserData (ProtocolData)
import Shared.TestnetConfig (mkTestnetNamiConfig)

runGetUserRelatedFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
runGetUserRelatedFundraisings onComplete onError protocolData = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig (getUserRelatedFundraisings protocolData)
  where
  handler :: Either Error (Array FundraisingInfo) -> Effect Unit
  handler (Right response) = onComplete response
  handler (Left err) = onError $ message err

getUserRelatedFundraisings :: ProtocolData -> Contract (Array FundraisingInfo)
getUserRelatedFundraisings protocolData = do
  allFrs <- getAllFundraisings protocolData
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  let userFrs = filterByPkh ownPkh allFrs
  logInfo' $ "Discovered items: " <> show userFrs
  pure userFrs
