module Governance.UserData where

import Contract.Prelude

import Data.BigInt (BigInt)

newtype StartGovernanceData = StartGovernanceData
  { quorum :: BigInt
  , fee :: BigInt
  }

derive newtype instance Show StartGovernanceData
derive newtype instance Eq StartGovernanceData
