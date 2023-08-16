module Governance.Config where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Value (TokenName)
import Contract.Value as Value
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson)
import Data.Argonaut.Encode (encodeJson)
import Effect (Effect)
import Ext.Data.Either (eitherM)
import Ext.Seriaization.Token (deserializeCurrency, deserializeTokenName)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

type GovernanceConfig =
  { governanceCurrency :: String
  , governanceTokenName :: String
  }

readGovernanceConfig :: Effect GovernanceConfig
readGovernanceConfig = do
  content <- readTextFile UTF8 "conf/governance.conf"
  jsonContent <- liftEffect $ eitherM "Can't parse governance config file: " $ parseJson content
  (governanceConfig :: GovernanceConfig) <- liftEffect $ eitherM "Can't decode GovernanceConfig: " $ (decodeJson jsonContent)
  pure governanceConfig

writeGovernanceConfig :: GovernanceConfig -> Effect Unit
writeGovernanceConfig governanceConfig = do
  let jGovernanceConfig = stringify $ encodeJson governanceConfig
  liftEffect $ writeTextFile UTF8 "conf/governance.conf" jGovernanceConfig

getGovTokenFromConfig ∷ Contract (Value.CurrencySymbol /\ TokenName)
getGovTokenFromConfig = do
  conf <- liftEffect $ readGovernanceConfig
  cs <- deserializeCurrency conf.governanceCurrency
  tn <- deserializeTokenName conf.governanceTokenName
  pure (cs /\ tn)
