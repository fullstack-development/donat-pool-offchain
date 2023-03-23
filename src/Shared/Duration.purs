module Shared.Duration where

import Contract.Prelude

import Data.BigInt  (BigInt, fromInt)
import Contract.Time (POSIXTime(..))

newtype Duration = Duration {
  days :: Int,
  hours :: Int,
  minutes :: Int
}
derive newtype instance Show Duration
derive newtype instance Eq Duration

minutesInDay :: BigInt
minutesInDay = fromInt 1440

minutesInHour :: BigInt
minutesInHour = fromInt 60

durationToMinutes :: Duration -> BigInt
durationToMinutes (Duration {days, hours, minutes}) =
    (fromInt days * minutesInDay) + (fromInt hours * minutesInHour) + fromInt minutes

-- minutesToDuration :: BigInt -> Duration
-- minutesToDuration minutesDuration = 
--     let days = minutesDuration `div` minutesInDay
--         remainingMinutes = minutesDuration - (days * minutesInDay)
--         hours = remainingMinutes `div` minutesInHour
--         minutes = remainingMinutes - (hours * minutesInHour)
--     in Duration {days: days, hours: hours, minutes}

daysDurationToMinutes :: BigInt -> BigInt
daysDurationToMinutes days = days * minutesInDay

minutesToPosixTime :: BigInt -> POSIXTime
minutesToPosixTime minutes =
  let
    milliseconds = minutes * fromInt 60 * fromInt 1000
  in
    POSIXTime milliseconds
