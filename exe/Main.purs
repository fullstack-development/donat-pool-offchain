-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Scaffold.Main (main, Contracts) where

import Contract.Prelude

import Common.ConnectWallet as ConnectWallet
import Data.BigInt (BigInt)
import Fundraising.Create as CreateFundraising
import Fundraising.Donate as Donate
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
  { connectWallet :: Effect Unit
  , startProtocol :: (Protocol -> Effect Unit) -> (String -> Effect Unit) -> ProtocolConfigParams -> Effect Unit
  , updateProtocol :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> ProtocolConfigParams -> Effect Unit
  , closeProtocol :: Protocol -> Effect Unit
  , getProtocolInfo :: (ProtocolConfigParams -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> Effect Unit
  , createFundraising :: (FundraisingData -> Effect Unit) -> (String -> Effect Unit) -> Protocol -> CreateFundraisingParams -> Effect Unit
  , donate :: FundraisingData -> BigInt -> Effect Unit
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
  , getAllFundraisings: AllFundraisings.runGetAllFundraisings
  , getUserRelatedFundraisings: UserRelatedFundraisings.runGetUserRelatedFundraisings
  }
