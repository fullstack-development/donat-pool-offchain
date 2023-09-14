module StakingPool.OpenNewEpoch where

import Contract.Prelude

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Chain (currentTime)
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract)
import Contract.TxConstraints as Constraints
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Ext.Contract.Time (Epoch, posixToTimeStamp)
import Protocol.Models (Protocol)
import Protocol.ProtocolScriptInfo (getProtocolManagerPkh, getProtocolScriptInfo)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds)
import Shared.ScriptInfo (ScriptInfo(..), getStakingPoolScriptInfo)
import Shared.Tx (completeTx)
import StakingPool.Constraints as StakingPool
import StakingPool.Datum (PStakingPoolDatum)
import StakingPoolInfo.Constraints as StakingPoolInfo

runOpenNewEpochFromCli :: Effect Unit
runOpenNewEpochFromCli = do
  protocolConfig <- readProtocolConfig
  let protocolData = mapToProtocolData protocolConfig
  launchAff_ $ runContract testnetKeyWalletConfig (openNewEpoch protocolData)

openNewEpoch :: ProtocolData -> Contract Unit
openNewEpoch protocolData = do
  logInfo' "Checking the need to open a new StakingPool epoch"
  protocol <- dataToProtocol protocolData

  now <- currentTime
  let timestamp = posixToTimeStamp now
  info'@(ScriptInfo info) <- getStakingPoolScriptInfo protocol
  let currentEpoch = (unwrap info.datum).currentEpoch
  if currentEpoch == timestamp.epoch then
    logInfo' "No need to open a new epoch"
  else
    openNewEpoch' protocol timestamp.epoch info'

openNewEpoch' :: Protocol -> Epoch -> ScriptInfo PStakingPoolDatum -> Contract Unit
openNewEpoch' protocol newEpoch spScriptInfo = do
  logInfo' "Opening a new StakingPool epoch..."
  ownCreds@(OwnCredentials creds) <- getOwnCreds
  protocolInfo <- getProtocolScriptInfo protocol
  managerPkh <- getProtocolManagerPkh protocolInfo
  when (creds.ownPkh /= managerPkh) $ liftEffect $ throw "No permissions to open a new epoch"

  spiConstraints <- StakingPoolInfo.mkOpenNewEpochConstraints protocol newEpoch protocolInfo
  let spConstraints /\ spLookups = StakingPool.mkOpenNewEpochConstraints spScriptInfo newEpoch

  let
    constraints =
      spConstraints
        <> spiConstraints
        <> Constraints.mustBeSignedBy managerPkh

  completeTx spLookups constraints ownCreds

  logInfo' "The new StakingPool epoch opened successfully"
