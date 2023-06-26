module Config.Protocol where

import Contract.Prelude

import Contract.Address (Bech32String)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson)
import Data.Argonaut.Encode (encodeJson)
import Effect (Effect)
import Ext.Data.Either (eitherM)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Protocol.UserData (ProtocolData(..))

type ProtocolConfig =
  { protocolCurrency :: Bech32String
  , protocolTokenName :: String
  }

readProtocolConfig :: Effect ProtocolConfig
readProtocolConfig = do
  content <- readTextFile UTF8 "conf/protocol.local.conf"
  jsonContent <- liftEffect $ eitherM "Can't parse protocol config from file: " $ parseJson content
  (protocolConfig :: ProtocolConfig) <- liftEffect $ eitherM "Can't decode ServicesConf: " $ (decodeJson jsonContent)
  log $ "ProtocolConfig: " <> show protocolConfig
  pure protocolConfig

writeProtocolConfig :: ProtocolConfig -> Effect Unit
writeProtocolConfig protocolConfig = do
  let jProtocolConfig = stringify $ encodeJson protocolConfig
  liftEffect $ writeTextFile UTF8 "conf/protocol.local.conf" jProtocolConfig

mapFromProtocolData :: ProtocolData -> ProtocolConfig
mapFromProtocolData (ProtocolData { protocolCurrency, protocolTokenName }) =
  { protocolCurrency: protocolCurrency
  , protocolTokenName: protocolTokenName
  }

mapToProtocolData :: ProtocolConfig -> ProtocolData
mapToProtocolData protocolConfig = ProtocolData
  { protocolCurrency: protocolConfig.protocolCurrency
  , protocolTokenName: protocolConfig.protocolTokenName
  }
