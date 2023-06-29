module Shared.Config where

import Contract.Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson)
import Data.Argonaut.Encode (encodeJson)
import Data.BigInt (fromNumber, toNumber)
import Effect (Effect)
import Ext.Data.Either (eitherM)
import Ext.Data.Maybe (maybeM)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Protocol.UserData (ProtocolConfigParams(..))

type DonatPoolConfig =
  { minAmountParam :: Number
  , maxAmountParam :: Number
  , minDurationParam :: Number
  , maxDurationParam :: Number
  , protocolFeeParam :: Number
  }

readDonatPoolConfig :: Effect DonatPoolConfig
readDonatPoolConfig = do
  content <- readTextFile UTF8 "conf/config.local.conf"
  jsonContent <- liftEffect $ eitherM "Can't parse donat pool configuration from config file content: " $ parseJson content
  (donatPoolConfig :: DonatPoolConfig) <- liftEffect $ eitherM "Can't decode DonatPoolConfig: " $ (decodeJson jsonContent)
  log $ "DonatPoolConfig: " <> show donatPoolConfig
  pure donatPoolConfig

writeDonatPoolConfig :: DonatPoolConfig -> Effect Unit
writeDonatPoolConfig donatPoolConfig = do
  let jDonatPoolConfig = stringify $ encodeJson donatPoolConfig
  liftEffect $ writeTextFile UTF8 "conf/config.local.conf" jDonatPoolConfig

mapFromProtocolConfigParams :: ProtocolConfigParams -> DonatPoolConfig
mapFromProtocolConfigParams (ProtocolConfigParams params) =
  { minAmountParam: toNumber params.minAmountParam
  , maxAmountParam: toNumber params.maxAmountParam
  , minDurationParam: toNumber params.minDurationParam
  , maxDurationParam: toNumber params.maxDurationParam
  , protocolFeeParam: toNumber params.protocolFeeParam
  }

mapToProtocolConfigParams :: DonatPoolConfig -> Effect ProtocolConfigParams
mapToProtocolConfigParams conf = do
  minAmountParam <- maybeM "Can't convert minAmountParam to BigInt" $ fromNumber conf.minAmountParam
  maxAmountParam <- maybeM "Can't convert maxAmountParam to BigInt" $ fromNumber conf.maxAmountParam
  minDurationParam <- maybeM "Can't convert minDurationParam to BigInt" $ fromNumber conf.minDurationParam
  maxDurationParam <- maybeM "Can't convert maxDurationParam to BigInt" $ fromNumber conf.maxDurationParam
  protocolFeeParam <- maybeM "Can't convert protocolFeeParam to BigInt" $ fromNumber conf.protocolFeeParam
  pure $ ProtocolConfigParams
    { minAmountParam: minAmountParam
    , maxAmountParam: maxAmountParam
    , minDurationParam: minDurationParam
    , maxDurationParam: maxDurationParam
    , protocolFeeParam: protocolFeeParam
    }
