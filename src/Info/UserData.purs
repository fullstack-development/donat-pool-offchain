module Info.UserData where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, Bech32String)
import Contract.Time (POSIXTime)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (ByteArray(..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.TextDecoder (decodeUtf8)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingTokenName)
import Protocol.UserData (ProtocolConfigParams)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO)
import Ext.Contract.Value (getCurrencyByTokenName, currencySymbolToString)
import Shared.MinAda (minAdaValue)

newtype FundraisingInfo = FundraisingInfo
  { creator :: PaymentPubKeyHash
  , description :: String
  , goal :: BigInt -- Goal in lovelaces
  , raisedAmt :: BigInt -- Raised amount in lovelaces
  , deadline :: POSIXTime
  , threadTokenCurrency :: Value.CurrencySymbol
  , threadTokenName :: Value.TokenName
  , path :: String
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
  let pathStr = currencySymbolToString cs
  pure $ FundraisingInfo
    { creator: currentDatum.creatorPkh
    , description: desc
    , goal: currentDatum.frAmount
    , raisedAmt: currentFunds
    , deadline: currentDatum.frDeadline
    , threadTokenCurrency: cs
    , threadTokenName: frTokenName
    , path: pathStr
    }

filterByPkh :: PaymentPubKeyHash -> Array FundraisingInfo -> Array FundraisingInfo
filterByPkh pkh = Array.filter belongsToUser
  where
  belongsToUser (FundraisingInfo frInfo) = frInfo.creator == pkh

newtype UserInfo = UserInfo
  { address :: Bech32String
  , isManager :: Boolean
  }

derive newtype instance Show UserInfo
derive newtype instance Eq UserInfo

newtype AppInfo = AppInfo
  { protocolConfig :: ProtocolConfigParams
  , userInfo :: UserInfo
  }

derive newtype instance Show AppInfo
derive newtype instance Eq AppInfo
