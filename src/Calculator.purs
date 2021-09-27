module Vacate.Calculator where

import Vacate.Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Int as Int
import Data.Time.Duration (Hours(..))
import MonthDate (MonthDate)
import MonthDate as MonthDate

vacationAccrued :: MonthDate -> MonthDate -> Hours 
vacationAccrued startDate endDate = Hours $ Int.toNumber $ (nMonths * 10 + nDiscretionaryDays * 8)
  where 
  nMonths = MonthDate.diff endDate startDate
  nDiscretionaryDays = 
    enumFromTo startDate endDate 
    # Array.drop 1 
    # map (unwrap >>> _.month >>> discretionaryDaysThisMonth)
    # sum

  discretionaryDaysThisMonth m = if Array.elem m [February, March, April, June, August, October] then 1 else 0



