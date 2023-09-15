module StakingPool.OpenNewEpoch where

import Contract.Prelude

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Chain (currentTime)
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime)
import Contract.TxConstraints as Constraints
import Ctl.Internal.Types.Interval (from)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Ext.Contract.Time (posixToTimeStamp, roundToSecond)
import Protocol.Models (Protocol)
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolManagerPkh, getProtocolScriptInfo)
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

  now' <- currentTime
  let
    now = roundToSecond now'
    timestamp = posixToTimeStamp now
  info'@(ScriptInfo info) <- getStakingPoolScriptInfo protocol
  let currentEpoch = (unwrap info.datum).currentEpoch
  if currentEpoch == timestamp.epoch then
    logInfo' "No need to open a new epoch"
  else
    openNewEpoch' protocol now info'

openNewEpoch' :: Protocol -> POSIXTime -> ScriptInfo PStakingPoolDatum -> Contract Unit
openNewEpoch' protocol now spScriptInfo = do
  logInfo' "Opening a new StakingPool epoch..."
  ownCreds@(OwnCredentials creds) <- getOwnCreds
  protocolInfo@(ProtocolScriptInfo protocolInfo') <- getProtocolScriptInfo protocol
  managerPkh <- getProtocolManagerPkh protocolInfo
  when (creds.ownPkh /= managerPkh) $ liftEffect $ throw "No permissions to open a new epoch"

  let
    timestamp = posixToTimeStamp now
    timeRange = from now
  spiConstraints <- StakingPoolInfo.mkOpenNewEpochConstraints protocol timestamp.epoch protocolInfo
  let spConstraints /\ spLookups = StakingPool.mkOpenNewEpochConstraints spScriptInfo timestamp.epoch

  let
    constraints =
      spConstraints
        <> spiConstraints
        <> Constraints.mustBeSignedBy managerPkh
        <> Constraints.mustValidateIn timeRange
        <> Constraints.mustReferenceOutput (fst protocolInfo'.pUtxo)
    lookups =
      spLookups
        <> Lookups.unspentOutputs protocolInfo'.pUtxos

  completeTx lookups constraints ownCreds

  logInfo' "The new StakingPool epoch opened successfully"
