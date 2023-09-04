module Info.UserData where

import Contract.Prelude

import Contract.Address (Bech32String, addressToBech32)
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftContractM)
import Contract.Time (POSIXTime)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (ByteArray(..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.TextDecoder (decodeUtf8)
import Ext.Contract.Value (getCurrencyByTokenName, currencySymbolToString)
import Ext.Data.Either (eitherContract)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingTokenNameString, getFundraisingTokenName)
import Protocol.UserData (ProtocolConfigParams)
import Shared.MinAda (minAdaValue)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO)

newtype FundraisingInfo = FundraisingInfo
  { creator :: Bech32String
  , title :: String
  , goal :: BigInt -- Goal in lovelaces
  , raisedAmt :: BigInt -- Raised amount in lovelaces
  , deadline :: POSIXTime
  , threadTokenCurrency :: String
  , threadTokenName :: String
  , isCompleted :: Boolean
  }

derive newtype instance Show FundraisingInfo
derive newtype instance Eq FundraisingInfo

mapToFundraisingInfo :: UtxoTuple -> Contract FundraisingInfo
mapToFundraisingInfo utxo = do
  PFundraisingDatum currentDatum <- liftContractM "Impossible to extract datum from UTxO" $ extractDatumFromUTxO utxo
  let frVal = extractValueFromUTxO utxo
  let currentFunds = Value.valueToCoin' frVal - Value.valueToCoin' minAdaValue - Value.valueToCoin' minAdaValue
  let ByteArray unwrappedTitle = currentDatum.frTitle
  title <- eitherContract "Title decoding failed: " $ decodeUtf8 unwrappedTitle
  frTokenName <- getFundraisingTokenName
  cs <- liftContractM "Impossible to get currency by token name" $ getCurrencyByTokenName frVal frTokenName
  now <- currentTime
  creator <- addressToBech32 currentDatum.creator
  pure $ FundraisingInfo
    { creator: creator
    , title: title
    , goal: currentDatum.frAmount
    , raisedAmt: currentFunds
    , deadline: currentDatum.frDeadline
    , threadTokenCurrency: currencySymbolToString cs
    , threadTokenName: fundraisingTokenNameString
    , isCompleted: now > currentDatum.frDeadline || currentFunds >= currentDatum.frAmount
    }

filterByCreatorAddress :: Bech32String -> Array FundraisingInfo -> Array FundraisingInfo
filterByCreatorAddress address = Array.filter belongsToUser
  where
  belongsToUser (FundraisingInfo frInfo) = frInfo.creator == address

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
