module Fundraising.UserData where

import Contract.Prelude

newtype CreateFundraisingParams = CreateFundraisingParams
  { description :: String -- 35 symbols max
  , amount :: Int -- amount to raise in Ada (not Lovelace)
  , duration :: Int -- Fundraising duration in days
  }

derive newtype instance Show CreateFundraisingParams
derive newtype instance Eq CreateFundraisingParams
