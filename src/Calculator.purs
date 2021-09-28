module Vacate.Calculator (discretionaryDaysThisMonth, holidaysThisMonth, vacationAccrued) where

import Vacate.Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Time.Duration (Hours(..))
import MonthDate (MonthDate)
import MonthDate as MonthDate

type Vacation = { vacationHours :: Hours, discretionaryHours :: Hours }

monthsWithoutHolidays :: Array Month
monthsWithoutHolidays = [February, March, April, June, August, October]

discretionaryDaysThisMonth :: Month -> Int
discretionaryDaysThisMonth m = if Array.elem m monthsWithoutHolidays then 1 else 0

holidaysThisMonth :: Month -> Int
holidaysThisMonth m = if Array.elem m monthsWithoutHolidays then 0 else 1

vacationAccrued :: MonthDate -> MonthDate -> Vacation 
vacationAccrued startDate endDate = { vacationHours, discretionaryHours }
  where 
  vacationHours = Hours $ Int.toNumber $ (nMonths * 10)
  nMonths = MonthDate.diff endDate startDate

  discretionaryHours = 
    enumFromTo startDate endDate 
    # Array.drop 1
    # Array.filter (unwrap >>> _.year >>> (_ == yearForDiscretionaryDays))
    # Array.filter (unwrap >>> _.month >>> (_ `Array.elem` monthsWithoutHolidays))
    # map (const $ Hours 8.0) 
    # fold

  yearForDiscretionaryDays = unwrap endDate # _.year



