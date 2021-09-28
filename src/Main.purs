module Main where

import Vacate.Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get)
import Data.Array (many)
import Data.Foldable (for_)
import Data.Formatter.Parser.Number (parseInteger)
import Data.Int as Int
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Now (nowDate)
import MonthDate (MonthDate(..), prettyPrint)
import MonthDate as MonthDate
import Node.ReadLine.Aff (Interface)
import Node.ReadLine.Aff as RL
import Text.Parsing.Parser (ParseError(..), ParseState(..), Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.String (skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum)
import Vacate.Calculator (holidaysThisMonth, vacationAccrued)

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

parseMonth :: String -> Maybe Month
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

printStats :: ∀ m. MonadEffect m => UserInput -> m Unit
printStats { hoursSoFar, dateInTheFuture } = do
  today <- liftEffect nowDate
  let thisMonth = MonthDate.fromDate today
  for_ (enumFromTo thisMonth dateInTheFuture :: Array _) \monthDate@(MonthDate { month }) -> do
    let 
      vacation = vacationAccrued thisMonth monthDate <> { vacationHours: Hours hoursSoFar, discretionaryHours: Hours 0.0 }
      msg = i
        (prettyPrint monthDate)": "(show $ un Hours vacation.vacationHours)" hours of vacation; "
        (show $ un Hours vacation.discretionaryHours)" discretionary hours"
        (if holidaysThisMonth month > 0 then i"; ("(show $ holidaysThisMonth month)" holidays)." else "")
    log msg

parseVacationTime :: ∀ m. MonadError String m => String -> m { nHours :: Hours, month :: MonthDate }
parseVacationTime inputStr = liftError $ runParser inputStr do 
  nHours <- (Hours <<< Int.toNumber) <$> parseInteger
  _ <- string " hours in " <|> string " hrs in "
  month <- parseMonthDate
  pure { nHours, month }

  where
  liftError (Left err) = throwError $ parseErrorMessage err
  liftError (Right err) = pure err

  parseMonthDate :: Parser String MonthDate
  parseMonthDate = do
    month <- parseMonth'
    skipSpaces
    year <- parseYear
    pure $ MonthDate { month, year }

  parseMonth' :: Parser String Month
  parseMonth' = do
    monthStr <- word
    (ParseState _ pos _) <- get
    case parseMonth monthStr of
      Nothing -> throwError (ParseError (i"Can't parse '"monthStr"' as a month") pos) 
      Just x -> pure x

  parseYear :: Parser String Year
  parseYear = do
    yearInt <- parseInteger
    (ParseState _ pos' _) <- get
    case toEnum yearInt of 
      Nothing -> throwError (ParseError (i(show yearInt)" is out of range of valid years") pos') 
      Just x -> pure x

  word :: Parser String String
  word = do
    chars <- many alphaNum
    pure $ String.fromCodePointArray $ (String.codePointFromChar <$> chars)


getVacationTime :: ∀ m. MonadAff m => Interface -> ExceptT String m { nHours :: Hours, month :: MonthDate }  
getVacationTime interface = do
  log "Any vacations to log? (e.g., 8 hours in May 2028)"
  ans <- RL.prompt interface
  parseVacationTime ans

applyAnyVacations :: Interface -> Aff Unit
applyAnyVacations interface = forever $ do
  parseResults <- runExceptT (getVacationTime interface)
  case parseResults of 
    Left err -> log err
    Right { nHours, month } -> log (i"You selected '"(show nHours)"' and '"(show month)"'")

main :: Effect Unit
main = launchAff_ do
  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface
  log "How many vacation hours do you have left right now?"
  hours <- RL.prompt interface
  log "How far into the future are you trying to plan? (Month/Year e.g., January 2025)"
  date <- RL.prompt interface

  case parseInput hours date of 
    Left err -> log err
    Right input -> printStats input

  RL.close interface

