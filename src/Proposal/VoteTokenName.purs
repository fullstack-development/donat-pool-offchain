module Proposal.VoteTokenName where

import Contract.Prelude

import Contract.Value as Value
import Data.BigInt (BigInt, toString)
import Ext.Contract.Value (mkTokenName)

mkVoteTokenName :: BigInt -> BigInt -> Maybe Value.TokenName
mkVoteTokenName vote amount = mkTokenName $ "D" <> toString vote <> "." <> toString amount
