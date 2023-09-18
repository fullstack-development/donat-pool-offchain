module StakingPool.Models where

import Contract.Prelude

import Contract.PlutusData (class FromData, class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), I, PNil, Z, genericFromData, genericToData)
import Contract.Monad (Contract)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Newtype (class Newtype)
import Ext.Contract.Value (mkCurrencySymbol)
import Governance.Config (getGovTokenFromConfig)
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.Models (Protocol)

newtype StakingPool = StakingPool
  { protocol :: Protocol
  , verTokenCurrency :: CurrencySymbol
  , govCurrency :: CurrencySymbol
  , govTokenName :: TokenName
  }

derive newtype instance Show StakingPool
derive instance Generic StakingPool _
derive instance Newtype StakingPool _

instance
  HasPlutusSchema
    StakingPool
    ( "StakingPool"
        :=
          ( "protocol" := I Protocol
              :+ "verTokenCurrency"
              := I CurrencySymbol
              :+ "govCurrency"
              := I CurrencySymbol
              :+ "govTokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

derive newtype instance Eq StakingPool
derive newtype instance Ord StakingPool

instance ToData StakingPool where
  toData = genericToData

instance FromData StakingPool where
  fromData = genericFromData

mkStakingPoolFromProtocol :: Protocol -> Contract StakingPool
mkStakingPoolFromProtocol protocol = do
  _ /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  (govCurrency /\ govTokenName) <- getGovTokenFromConfig
  pure $ StakingPool
    { protocol: protocol
    , verTokenCurrency: verTokenCs
    , govCurrency: govCurrency
    , govTokenName: govTokenName
    }
