-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main, Contracts) where

import Contract.Prelude

import Contract.Address (Bech32String)
import Common.ConnectWallet as ConnectWallet
import Fundraising.Donate as Donate
import Fundraising.ReceiveFunds as ReceiveFunds
import Data.BigInt (BigInt)
import Fundraising.Create as CreateFundraising
import Fundraising.UserData (CreateFundraisingParams, FundraisingData)
import Info.AllFundraisings as AllFundraisings
import Info.Protocol as ProtocolInfo
import Info.UserData (FundraisingInfo)
import Info.UserRelatedFundraisings as UserRelatedFundraisings
import Protocol.CloseProtocol as CloseProtocol
import Protocol.Models (Protocol)
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol
import Protocol.UserData (ProtocolConfigParams)

data Contracts = Contracts
  { connectWallet :: (Bech32String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
  , startProtocol :: (Protocol -> Effect Unit) -> (String -> Effect Unit) -> ProtocolConfigParams -> Effect Unit
  , updateProtocol :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> ProtocolConfigParams -> Effect Unit
  , closeProtocol :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
  , getProtocolInfo :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
  , createFundraising :: (FundraisingData -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> CreateFundraisingParams -> Effect Unit
  , donate :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> FundraisingData -> BigInt -> Effect Unit
  , receiveFunds :: (Unit -> Effect Unit) -> (String -> Effect Unit) -> FundraisingData -> Effect Unit
  , getAllFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
  , getUserRelatedFundraisings :: (Array FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
  }

main :: Contracts
main = Contracts
  { connectWallet: ConnectWallet.runConnectWallet
  , startProtocol: StartProtocol.runStartProtocol
  , updateProtocol: UpdateProtocol.runUpdateProtocol
  , closeProtocol: CloseProtocol.runCloseProtocolTest
  , getProtocolInfo: ProtocolInfo.runGetProtocolInfo
  , createFundraising: CreateFundraising.runCreateFundraising
  , donate: Donate.runDonate
  , receiveFunds: ReceiveFunds.runReceiveFunds
  , getAllFundraisings: AllFundraisings.runGetAllFundraisings
  , getUserRelatedFundraisings: UserRelatedFundraisings.runGetUserRelatedFundraisings
  }
