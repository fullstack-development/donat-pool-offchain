-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main, Contracts) where

import Contract.Prelude

import Common.ConnectWallet as ConnectWallet
import Contract.Address (Bech32String)
import Fundraising.Create as CreateFundraising
import Fundraising.Donate as Donate
import Fundraising.ReceiveFunds as ReceiveFunds
import Fundraising.UserData (CreateFundraisingParams, FundraisingData)
import Info.AllFundraisings as AllFundraisings
import Info.AppInfo as AppInfo
import Info.UserData (AppInfo, FundraisingInfo)
import Info.UserRelatedFundraisings as UserRelatedFundraisings
import Protocol.CloseProtocol as CloseProtocol
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol
import Protocol.UserData (ProtocolConfigParams, ProtocolData)

data Contracts = Contracts
  { connectWallet :: (Bech32String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
  , startProtocol :: (ProtocolData -> Effect Unit) -> (String -> Effect Unit) -> ProtocolConfigParams -> Effect Unit
  , updateProtocol :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> ProtocolConfigParams -> Effect Unit
  , closeProtocol :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
  , getAppInfo :: (AppInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
  , createFundraising :: (FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> CreateFundraisingParams -> Effect Unit
  , donate :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> FundraisingData -> Int -> Effect Unit
  , receiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> FundraisingData -> Effect Unit
  , getAllFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
  , getUserRelatedFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> Effect Unit
  }

main :: Contracts
main = Contracts
  { connectWallet: ConnectWallet.runConnectWallet
  , startProtocol: StartProtocol.runStartProtocol
  , updateProtocol: UpdateProtocol.runUpdateProtocol
  , closeProtocol: CloseProtocol.runCloseProtocolTest
  , getAppInfo: AppInfo.runGetAppInfo
  , createFundraising: CreateFundraising.runCreateFundraising
  , donate: Donate.runDonate
  , receiveFunds: ReceiveFunds.runReceiveFunds
  , getAllFundraisings: AllFundraisings.runGetAllFundraisings
  , getUserRelatedFundraisings: UserRelatedFundraisings.runGetUserRelatedFundraisings
  }
