module Info.UserData where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Time (POSIXTime)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (ByteArray(..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingTokenName)
import Shared.Helpers (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO, getCurrencyByTokenName)
import Shared.MinAda (minAdaValue)
import Data.TextDecoder (decodeUtf8)

newtype FundraisingInfo = FundraisingInfo
  { creator :: PaymentPubKeyHash
  , description :: String
  , goal :: BigInt -- Goal in lovelaces
  , raisedAmt :: BigInt -- Raised amount in lovelaces
  , deadline :: POSIXTime
  , threadTokenCurrency :: Value.CurrencySymbol
  , threadTokenName :: Value.TokenName
  }

derive newtype instance Show FundraisingInfo
derive newtype instance Eq FundraisingInfo

mapToFundraisingInfo :: UtxoTuple -> Maybe FundraisingInfo
mapToFundraisingInfo utxo = do
  PFundraisingDatum currentDatum <- extractDatumFromUTxO utxo
  let frVal = extractValueFromUTxO utxo
  let currentFunds = Value.valueToCoin' frVal - Value.valueToCoin' minAdaValue - Value.valueToCoin' minAdaValue
  let ByteArray unwrappedDesc = currentDatum.frDesc
  desc <- either (const Nothing) Just $ decodeUtf8 unwrappedDesc
  frTokenName <- fundraisingTokenName
  cs <- getCurrencyByTokenName frVal frTokenName
  pure $ FundraisingInfo
    { creator: currentDatum.creatorPkh
    , description: desc
    , goal: currentDatum.frAmount
    , raisedAmt: currentFunds
    , deadline: currentDatum.frDeadline
    , threadTokenCurrency: cs
    , threadTokenName: frTokenName
    }

filterByPkh :: PaymentPubKeyHash -> Array FundraisingInfo -> Array FundraisingInfo
filterByPkh pkh = Array.filter belongsToUser
  where
  belongsToUser (FundraisingInfo frInfo) = frInfo.creator == pkh
