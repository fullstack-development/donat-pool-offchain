module Protocol.UserData where

import Contract.Prelude

import Ctl.Internal.Types.Aliases (Bech32String)
import Data.BigInt (BigInt)
import Protocol.Datum (PProtocolDatum(..))
import Protocol.Models (PProtocolConfig(..), Protocol(..))
import Contract.Monad (Contract, liftContractM)
import Ctl.Internal.Types.ByteArray (byteArrayFromAscii, hexToByteArray)
import Ctl.Internal.Plutus.Types.CurrencySymbol as CurrencySymbol
import Contract.Value as Value
import Ext.Contract.Value (currencySymbolToString, tokenNameToString)

newtype ProtocolConfigParams = ProtocolConfigParams
  { minAmountParam :: BigInt
  , maxAmountParam :: BigInt
  , minDurationParam :: BigInt -- minutes
  , maxDurationParam :: BigInt -- minutes
  , protocolFeeParam :: BigInt -- percentage
  }

derive newtype instance Show ProtocolConfigParams
derive newtype instance Eq ProtocolConfigParams

mapToProtocolConfig :: ProtocolConfigParams -> PProtocolConfig
mapToProtocolConfig (ProtocolConfigParams configParams) = do
  PProtocolConfig
    { minAmount: configParams.minAmountParam
    , maxAmount: configParams.maxAmountParam
    , minDuration: configParams.minDurationParam
    , maxDuration: configParams.maxDurationParam
    , protocolFee: configParams.protocolFeeParam
    }

getConfigFromProtocolDatum :: PProtocolDatum -> ProtocolConfigParams
getConfigFromProtocolDatum (PProtocolDatum datum) =
  ProtocolConfigParams
    { minAmountParam: datum.minAmount
    , maxAmountParam: datum.maxAmount
    , minDurationParam: datum.minDuration
    , maxDurationParam: datum.maxDuration
    , protocolFeeParam: datum.protocolFee
    }

newtype ProtocolData = ProtocolData
  { protocolCurrency :: Bech32String
  , protocolTokenName :: String
  }

derive newtype instance Show ProtocolData
derive newtype instance Eq ProtocolData

protocolToData :: Protocol -> Contract ProtocolData
protocolToData (Protocol protocol) = do
  let protocolCurrencyString = currencySymbolToString $ protocol.protocolCurrency
  protocolTnString <- liftContractM "Impossible to decode Protocol token name" $ tokenNameToString $ protocol.protocolTokenName
  pure $ ProtocolData { protocolCurrency: protocolCurrencyString, protocolTokenName: protocolTnString }

dataToProtocol :: ProtocolData -> Contract Protocol
dataToProtocol (ProtocolData pData) = do
  curSymbol <- liftContractM "Impossible to get protocol currency symbol" $ (hexToByteArray pData.protocolCurrency >>= CurrencySymbol.mkCurrencySymbol)
  tokenName <- liftContractM "Impossible to get protocol token name" $ (byteArrayFromAscii pData.protocolTokenName >>= Value.mkTokenName)
  pure $ Protocol { protocolCurrency: curSymbol, protocolTokenName: tokenName }
