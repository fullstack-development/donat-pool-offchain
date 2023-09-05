module Ext.Contract.Time where

import Contract.Prelude
import Contract.Time (POSIXTime(..))
import Data.BigInt as BigInt

addTimes :: POSIXTime -> POSIXTime -> POSIXTime
addTimes (POSIXTime time1) (POSIXTime time2) = POSIXTime (time1 + time2)

type OrdinalDay = BigInt.BigInt
type Epoch = BigInt.BigInt -- one epoch lasts `epochSize` days
type DayOfEpoch = BigInt.BigInt

type TimeStamp = { epoch :: Epoch, dayOfEpoch :: DayOfEpoch }

epochSize :: BigInt.BigInt
epochSize = BigInt.fromInt 100

msInDay :: BigInt.BigInt
msInDay = BigInt.fromInt 1000 * BigInt.fromInt (60 * 60 * 24)

toOrdinalDay :: POSIXTime -> OrdinalDay
toOrdinalDay (POSIXTime milliseconds) = milliseconds `BigInt.quot` msInDay

toEpoch :: OrdinalDay -> Epoch
toEpoch ordDay = ordDay `BigInt.quot` epochSize

toDayOfEpoch :: OrdinalDay -> DayOfEpoch
toDayOfEpoch ordDay = ordDay `BigInt.rem` epochSize

posixToTimeStamp :: POSIXTime -> TimeStamp
posixToTimeStamp posix =
  let
    ordDay = toOrdinalDay posix
  in
    { epoch: toEpoch ordDay, dayOfEpoch: toDayOfEpoch ordDay }
