module Main where

import Vacate.Prelude

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Interpolate (i)
import Data.Newtype (un)
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import MonthDate (MonthDate(..))
import MonthDate as MonthDate
import Node.ReadLine.Aff as RL
import Vacate.Calculator (vacationAccrued)

type UserInput = { hoursSoFar :: Number, dateInTheFuture :: MonthDate } 

parseInput :: String -> String -> Either String UserInput
parseInput hours date = do
  hoursSoFar <- Number.fromString hours # note (i"Unable to parse '"hours"' as a number")
  case String.split (Pattern " ") date of 
    [ monthStr, yearStr ] -> do
      month <- parseMonth monthStr # note (i"Unable to recognize '"monthStr"' as a month")
      yearInt <- Int.fromString yearStr # note (i"Unable to parse '"yearStr"' as a number") 
      year <- toEnum yearInt # note (i"Year '"yearStr"' is outside the bounds of known years")
      pure $ { hoursSoFar, dateInTheFuture: MonthDate { month, year } }
    _ -> Left $ i"Expecting 'Month Year', e.g., 'January 2025', but got '"date"'"

  where 
  parseMonth monthStr = case String.toLower monthStr of 
    "january" -> Just January
    "february" -> Just February
    "march" -> Just March
    "april" -> Just April
    "may" -> Just May
    "june" -> Just June
    "july" -> Just July
    "august" -> Just August
    "september" -> Just September
    "october" -> Just October
    "november" -> Just November
    "december" -> Just December

    "jan" -> Just January
    "feb" -> Just February
    "mar" -> Just March
    "apr" -> Just April
    "may" -> Just May
    "jun" -> Just June
    "jul" -> Just July
    "aug" -> Just August
    "sep" -> Just September
    "sept" -> Just September
    "oct" -> Just October
    "nov" -> Just November
    "dec" -> Just December

    _ -> Nothing


main :: Effect Unit
main = launchAff_ do
  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface
  log "How many vacation hours do you have left right now?"
  hours <- RL.prompt interface
  log "What Month/Year do you want to know about? (e.g., January 2025)"
  date <- RL.prompt interface

  case parseInput hours date of 
    Left err -> log err
    Right { hoursSoFar, dateInTheFuture } -> do
      today <- liftEffect nowDate
      let vacation = vacationAccrued (MonthDate.fromDate today) dateInTheFuture <> Hours hoursSoFar
      log (i"In "date", you will have "(show $ un Hours vacation)" hours of vacation.")

  RL.close interface

