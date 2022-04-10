module Vacate.Frontend.Main where

import Vacate.Frontend.Prelude

import Concur.React.DOM (button, div)
import Concur.React.Props as Props
import Concur.React.Widgets (textInputWithButton)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (many)
import Data.Array as Array
import Data.Formatter.Parser.Number (parseInteger, parseNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Effect.Now (nowDate)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum)
import Vacate.Shared.Calculator (Vacation, holidaysThisMonth, vacationStatsByMonth)
import Vacate.Shared.MonthDate (MonthDate(..), prettyPrint)
import Vacate.Shared.MonthDate as MonthDate

type UserInput = { vacationSoFar :: Number, discretionarySoFar :: Number, dateInTheFuture :: MonthDate }

parseInput :: String -> String -> String -> Either String UserInput
parseInput vacation discretionary date = do
  vacationSoFar <- Number.fromString vacation # note (i "Unable to parse '"vacation"' as a number")
  discretionarySoFar <- Number.fromString discretionary # note (i "Unable to parse '"discretionary"' as a number")

  case String.split (Pattern " ") date of
    [ monthStr, yearStr ] -> do
      month <- parseMonth monthStr # note (i "Unable to recognize '"monthStr"' as a month")
      yearInt <- Int.fromString yearStr # note (i "Unable to parse '"yearStr"' as a number")
      year <- toEnum yearInt # note (i "Year '"yearStr"' is outside the bounds of known years")
      pure $ { vacationSoFar, discretionarySoFar, dateInTheFuture: MonthDate { month, year } }
    _ -> Left $ i "Expecting 'Month Year', e.g., 'January 2025', but got '"date"'"


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

getInput :: Widget HTML UserInput
getInput = do
  vacation <- 
    ( text "How many vacation hours do you have left right now?"
    <|> textInputWithButton "" "Enter" [Props._id "vacation-button"] []
    )

  discretionary <-
    ( text "How many discretionary hours do you have left right now?"
    <|> textInputWithButton "" "Enter" [Props._id "discretionar-button"] []
    )

  date <- 
    ( text "How far into the future are you trying to plan? (Month/Year e.g., January 2025)"
    <|> textInputWithButton "" "Enter" [Props._id "future-button"] []
    )

  case parseInput vacation discretionary date of
    Right input -> pure input
    Left err -> do
      _ <- (text err <|> button [] [])
      getInput

printStats :: ∀ a. UserInput -> Array Vacation -> Widget HTML a
printStats { vacationSoFar, discretionarySoFar, dateInTheFuture } vacations = do
  today <- liftEffect nowDate
  let
    thisMonth = MonthDate.fromDate today
    thisMonthStats = { vacationHours: Hours vacationSoFar, discretionaryHours: Hours discretionarySoFar }
    allStats = vacationStatsByMonth vacations thisMonthStats thisMonth dateInTheFuture

  fold $ do
    (monthDate@(MonthDate { month }) /\ vacation) <- (Map.toUnfoldable allStats :: Array _)
    let
      vacationTaken = Array.filter (_.month >>> (==) monthDate) vacations # map (_.nHours) # fold # un Hours
      msg =
        i (prettyPrint monthDate)": "(un Hours vacation.vacationHours)" hours of vacation; "
          <> i (un Hours vacation.discretionaryHours)" discretionary hours"
          <> (if holidaysThisMonth month > 0 then i "; ("(holidaysThisMonth month)" holidays)" else "")
          <> (if vacationTaken > 0.0 then i "   *** "vacationTaken" hours vacation taken" else "")
          <> "."
    pure $ div [] [text msg]


parseVacationTime :: ∀ m. MonadError String m => String -> m Vacation
parseVacationTime inputStr = liftError $ runParser inputStr do
  nHours <- Hours <$> (parseNumber <* skipSpaces)
  try (string "hours" <|> string "hrs" <|> string "") *> skipSpaces
  try (string "in" <|> string "") *> skipSpaces
  month <- parseMonthDate
  pure { nHours, month }

  where
  parseMonthDate :: Parser String MonthDate
  parseMonthDate = do
    month <- parseMonth'
    skipSpaces
    year <- parseYear
    pure $ MonthDate { month, year }

  parseMonth' :: Parser String Month
  parseMonth' = do
    monthStr <- parseWord
    parseMaybeWith (defer \_ -> i "Can't parse '"monthStr"' as a month") parseMonth monthStr

  parseYear :: Parser String Year
  parseYear = do
    yearInt <- parseInteger
    parseMaybeWith (defer \_ -> i yearInt" is out of range of valid years") toEnum yearInt

  parseWord :: Parser String String
  parseWord = do
    chars <- many alphaNum
    pure $ String.fromCodePointArray $ map String.codePointFromChar $ chars

  parseMaybeWith :: ∀ stream a b. Lazy String -> (a -> Maybe b) -> a -> Parser stream b
  parseMaybeWith msg tryParse x =
    case tryParse x of
      Nothing -> fail $ force msg
      Just parsed -> pure parsed

  liftError (Left err) = throwError $ parseErrorMessage err
  liftError (Right err) = pure err

getVacationTime :: ExceptT String (Widget HTML) Vacation
getVacationTime = do
  ans <- lift $ 
    ( text "Any vacations to log? (e.g., 8 hours in May 2028)"
    <|> textInputWithButton "" "Enter" [Props._id "log-button"] []
    )
  parseVacationTime ans

applyAnyVacations :: Widget HTML (Either String { nHours :: Hours, month :: MonthDate }) -> UserInput -> Widget HTML Unit
applyAnyVacations initialStats userInput = evalStateT (forever go) {stats: initialStats, currentVacations: []}
  where
  go :: StateT ({ stats :: Widget HTML (Either String { nHours :: Hours, month :: MonthDate }), currentVacations :: Array { nHours :: Hours, month :: MonthDate }}) (Widget HTML) Unit
  go = do
    {stats, currentVacations} <- get
    parseResults <- lift (stats <|> runExceptT getVacationTime)
    case parseResults of
      Left err -> 
        ( text err
        <|> button [] [text "Try Again"]
        )
      Right vacation -> do
        let newVacations = currentVacations <> [ vacation ]
        put { stats: printStats userInput newVacations, currentVacations: newVacations }

content :: Widget HTML Unit
content = do
  input <- getInput
  applyAnyVacations (printStats input []) input


main :: Effect Unit
main = runWidgetInDom "contents" content