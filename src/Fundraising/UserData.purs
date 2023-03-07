module Fundraising.UserData where

import Contract.Prelude

import Contract.Value (CurrencySymbol, TokenName)
import Protocol.Models (Protocol)

newtype CreateFundraisingParams = CreateFundraisingParams
  { description :: String -- 35 symbols max
  , amount :: Int -- amount to raise in Ada (not Lovelace)
  , duration :: Int -- Fundraising duration in days
  }

derive newtype instance Show CreateFundraisingParams
derive newtype instance Eq CreateFundraisingParams

newtype FundraisingData = FundraisingData
  { protocol :: Protocol
  , frThreadTokenCurrency :: CurrencySymbol
  , frThreadTokenName :: TokenName
  }

derive newtype instance Show FundraisingData
derive newtype instance Eq FundraisingData
