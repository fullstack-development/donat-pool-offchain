module Fundraising.Datum where

import Contract.Address (PaymentPubKeyHash)
import Contract.Time (POSIXTime)
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Ctl.Internal.FromData
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord)

descLength :: Int
descLength = 35

newtype PFundraisingDatum = PFundraisingDatum
  { creatorPkh :: PaymentPubKeyHash
  , tokenOrigin :: TransactionInput
  , frTitle :: ByteArray --  descLength is set to limit the description size 
  , frAmount :: BigInt -- amount to raise in Lovelace
  , frDeadline :: POSIXTime
  , frFee :: BigInt -- percentage
  , managerPkh :: PaymentPubKeyHash
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
              :+ "frTitle"
              := I ByteArray
              :+ "frAmount"
              := I BigInt
              :+ "frDeadline"
              := I POSIXTime
              :+ "frFee"
              := I BigInt
              :+ "managerPkh"
              := I PaymentPubKeyHash
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData PFundraisingDatum where
  toData = genericToData

instance FromData PFundraisingDatum where
  fromData = genericFromData
