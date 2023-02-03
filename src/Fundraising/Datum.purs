module Fundraising.Datum where

import Ctl.Internal.FromData
import Contract.Address (PaymentPubKeyHash)
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Contract.Time (POSIXTime)

newtype PFundraisingDatum = PFundraisingDatum
  { creatorPkh :: PaymentPubKeyHash
  , tokenOrigin :: TransactionInput
  , frDesc :: ByteArray
  , frAmount :: BigInt
  , frDeadline :: POSIXTime
  , frFee :: BigInt -- percentage
  }

derive instance Generic PFundraisingDatum _
derive instance Newtype PFundraisingDatum _
derive newtype instance Show PFundraisingDatum
derive newtype instance Eq PFundraisingDatum
derive newtype instance Ord PFundraisingDatum

instance
  HasPlutusSchema
    PFundraisingDatum
    ( "PFundraisingDatum"
        :=
          ( "creatorPkh" := I PaymentPubKeyHash
              :+ "tokenOrigin"
              := I TransactionInput
              :+ "frDesc"
              := I ByteArray
              :+ "frAmount"
              := I BigInt
              :+ "frDeadline"
              := I POSIXTime
              :+ "frFee"
              := I BigInt
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PFundraisingDatum where
  toData = genericToData

instance FromData PFundraisingDatum where
  fromData = genericFromData
