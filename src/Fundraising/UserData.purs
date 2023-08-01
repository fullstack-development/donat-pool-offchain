module Fundraising.UserData where

import Contract.Prelude

import Info.UserData (FundraisingInfo(..))
import Shared.Duration (Duration)

newtype CreateFundraisingParams = CreateFundraisingParams
  { title :: String -- 35 symbols max
  , amount :: Int -- amount to raise in Ada (not Lovelace)
  , duration :: Duration
  }

derive newtype instance Show CreateFundraisingParams
derive newtype instance Eq CreateFundraisingParams

newtype FundraisingData = FundraisingData
  { frThreadTokenCurrency :: String
  , frThreadTokenName :: String
  }

derive newtype instance Show FundraisingData
derive newtype instance Eq FundraisingData

mapFundraisingInfoToData :: FundraisingInfo -> FundraisingData
mapFundraisingInfoToData (FundraisingInfo { threadTokenCurrency, threadTokenName }) =
  FundraisingData { frThreadTokenCurrency: threadTokenCurrency, frThreadTokenName: threadTokenName }