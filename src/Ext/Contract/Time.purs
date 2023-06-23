module Ext.Contract.Time where

import Contract.Prelude
import Contract.Time (POSIXTime(..))

addTimes :: POSIXTime -> POSIXTime -> POSIXTime
addTimes (POSIXTime time1) (POSIXTime time2) = POSIXTime (time1 + time2)
