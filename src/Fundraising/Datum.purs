module Fundraising.Datum where

import Ctl.Internal.FromData

import Contract.Address (PaymentPubKeyHash, Address)
import Contract.PlutusData (class HasPlutusSchema, type (:+), type (:=), type (@@), I, PNil, Z, genericToData)
import Contract.Prelude (class Generic, class Show)
import Contract.Time (POSIXTime)
import Ctl.Internal.ToData (class ToData)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Ord)

titleLength :: Int
titleLength = 35

newtype PFundraisingDatum = PFundraisingDatum
  { creatorPkh :: PaymentPubKeyHash
  , creatorAddress :: Address
  , tokenOrigin :: TransactionInput
  , frTitle :: ByteArray --  titleLength is set to limit the title size 
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
              :+ "creatorAddress"
              := I Address
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
