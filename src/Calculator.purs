module Vacate.Calculator where

import Vacate.Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (over2)
import MonthDate (MonthDate)

type Vacation = { nHours :: Hours, month :: MonthDate } 
type VacationStats = { vacationHours :: Hours, discretionaryHours :: Hours }

monthsWithoutHolidays :: Array Month
monthsWithoutHolidays = [February, March, April, June, August, October]

discretionaryDaysThisMonth :: Month -> Int
discretionaryDaysThisMonth m = if Array.elem m monthsWithoutHolidays then 1 else 0

holidaysThisMonth :: Month -> Int
holidaysThisMonth m = if Array.elem m monthsWithoutHolidays then 0 else 1

subHours :: Hours -> Hours -> Hours
subHours = over2 Hours (-)
infixr 5 subHours as .-

plusHours :: Hours -> Hours -> Hours
plusHours = (<>)
infixr 5 plusHours as .+

vacationStatsByMonth :: Array Vacation -> VacationStats -> MonthDate -> MonthDate -> Map MonthDate VacationStats
vacationStatsByMonth vacations currentStats startDate endDate = 
  enumFromTo startDate endDate 
  # Array.foldl vacationAccruedForMonth {rollover: currentStats, stats: Map.empty} 
  # _.stats

  where 

  vacationAccruedForMonth {rollover, stats} month = 
    { rollover: thisMonthStats, stats: Map.insert month thisMonthStats stats }

    where
    thisMonthStats = { vacationHours, discretionaryHours } 
    vacationHours = rollover.vacationHours .+ vacationAccrued .- vacationUsed 
    vacationAccrued 
      | month == startDate = Hours 0.0
      | otherwise = Hours 10.0
    vacationUsed = vacationRequested .- discretionaryUsed

    discretionaryHours = discretionaryAvailable .- discretionaryUsed
    discretionaryAvailable
      | (unwrap month).month == January = Hours 0.0
      | otherwise = rollover.discretionaryHours .+ discretionaryAccrued

    discretionaryAccrued  
      | month /= startDate && (unwrap month).month `Array.elem` monthsWithoutHolidays = Hours 8.0
      | otherwise = Hours 0.0

    discretionaryUsed = min vacationRequested discretionaryAvailable 

    vacationRequested = vacations # Array.find (_.month >>> (==) month) # map _.nHours # fromMaybe (Hours 0.0)




