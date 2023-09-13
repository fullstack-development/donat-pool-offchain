module StakingPool.Redeemer where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.PlutusData (class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), PNil, S, Z, genericToData)
import Data.BigInt (BigInt)

type ProviderPkh = PaymentPubKeyHash
type DaoTokensAmt = BigInt

data PStakingPoolRedeemer
  = PDepositWithCurrentEpoch DaoTokensAmt ProviderPkh
  | POpenNewEpoch
  | PWithdrawRewards -- not implemented for now
  | PWithdrawInFull -- not implemented for now

derive instance Generic PStakingPoolRedeemer _

instance
  HasPlutusSchema
    PStakingPoolRedeemer
    ( "PDepositWithCurrentEpoch" := PNil @@ Z
        :+ "POpenNewEpoch"
        := PNil
        @@ (S Z)
        :+ "PWithdrawRewards"
        := PNil
        @@ (S (S Z))
        :+ "PWithdrawInFull"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

instance ToData PStakingPoolRedeemer where
  toData = genericToData
