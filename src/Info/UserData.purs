module Info.UserData where

import Contract.Prelude

import Contract.Address (Bech32String)
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftContractM)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (ByteArray(..))
import Data.Array as Array
import Data.BigInt (toNumber)
import Data.TextDecoder (decodeUtf8)
import Ext.Contract.Value (getCurrencyByTokenName, currencySymbolToString)
import Ext.Data.Either (eitherContract)
import Ext.Seriaization.Key (pkhToBech32M)
import Fundraising.Datum (PFundraisingDatum(..))
import Fundraising.FundraisingScript (fundraisingTokenNameString, getFundraisingTokenName)
import Protocol.UserData (ProtocolConfigParams)
import Shared.MinAda (minAdaValue)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO)

newtype FundraisingInfo = FundraisingInfo
  { creator :: Maybe Bech32String
  , title :: String
  , goal :: Number -- Goal in lovelaces
  , raisedAmt :: Number -- Raised amount in lovelaces
  , deadline :: Number
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
  creator <- pkhToBech32M currentDatum.creatorPkh
  pure $ FundraisingInfo
    { creator: Just creator
    , title: title
    , goal: toNumber currentDatum.frAmount
    , raisedAmt: toNumber currentFunds
    , deadline: toNumber <<< unwrap $ currentDatum.frDeadline
    , threadTokenCurrency: currencySymbolToString cs
    , threadTokenName: fundraisingTokenNameString
    , isCompleted: now > currentDatum.frDeadline || currentFunds >= currentDatum.frAmount
    }

filterByPkh :: Bech32String -> Array FundraisingInfo -> Array FundraisingInfo
filterByPkh pkh = Array.filter belongsToUser
  where
  belongsToUser (FundraisingInfo frInfo) = isCreatorsPkh frInfo.creator
  isCreatorsPkh Nothing = false
  isCreatorsPkh (Just creator) = creator == pkh

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
