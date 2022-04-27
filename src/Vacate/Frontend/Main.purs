module Vacate.Frontend.Main where

import Vacate.Frontend.Prelude

import Concur.React.DOM as DOM
import Concur.React.Props (ReactProps)
import Concur.React.Props as Props
import Control.Monad.Except (class MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (many)
import Data.Array as Array
import Data.Formatter.Parser.Number (parseInteger, parseNumber)
import Data.Int as Int
import Data.Map as Map
import Data.String as String
import Effect.Now (nowDate)
import Option as Option
import React.Basic.DOM (css)
import React.SyntheticEvent (SyntheticMouseEvent)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum)
import Vacate.Frontend.MUI (ValueLabelDisplay(..), monthDatePicker)
import Vacate.Frontend.MUI as MUI
import Vacate.Shared.Calculator (Vacation, holidaysThisMonth, vacationStatsByMonth)
import Vacate.Shared.MonthDate (MonthDate(..), prettyPrint)
import Vacate.Shared.MonthDate as MonthDate

type UserInput = { vacationSoFar :: Number, discretionarySoFar :: Number, dateInTheFuture :: MonthDate }

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


textInputWithButton :: ∀ a.
  String ->
  String ->
  (∀ b. Array (ReactProps b)) ->
  MUI.ButtonProps a ->
  Widget HTML String
textInputWithButton val buttonlabel inpProps buttonProps = go val
  where
  go state = do
    input <-
      DOM.div'
        [ DOM.input $ inpProps <>
          [ Props.onChange <#> Props.unsafeTargetValue <#> Just 
          , Props.onKeyEnter $> Nothing
          , Props.value state
          ]
        , DOM.text " "
        , MUI.button (Option.alter {onClick: (\(_ :: Maybe (SyntheticMouseEvent -> a)) -> Just $ const Nothing)} buttonProps) [DOM.text buttonlabel]
        ]

    input # caseMaybe { just: go, nothing: pure state }


getInput :: Widget HTML UserInput
getInput = do
  {vacation, discretionary, date } <- selectVacationTimes
  pure {vacationSoFar: Int.toNumber vacation, discretionarySoFar: Int.toNumber discretionary, dateInTheFuture: date}

  where

  selectVacationTimes :: Widget HTML { vacation :: Int, discretionary :: Int, date :: MonthDate }
  selectVacationTimes = do
    thisMonth <- liftEffect $ MonthDate.fromDate <$> nowDate
    go { vacation: 0, discretionary: 0, date: thisMonth }

    where 
    go {vacation, discretionary, date} = do
      input <- 
        (   DOM.text "How many vacation hours do you have left right now?"
        <|> slider (Just <<< {vacation: _, discretionary, date})
        <|> DOM.text "How many discretionary hours do you have left right now?"
        <|> slider (Just <<< {vacation, discretionary: _, date})
        <|> DOM.text "How far into the future are you trying to plan?"
        <|> DOM.div
          [ Props.className "date-box" ]
          [ monthDatePicker 
            ( Option.fromRecord 
              { label: "Month & Year"
              , value: date
              , onChange: (Just <<< { vacation, discretionary, date: _ })
              }
            )
          ]
        <|> MUI.button (Option.fromRecord {onClick: const Nothing, style: css {display: "flex"}}) [DOM.text "Enter"]
        )

      input # caseMaybe { just: go, nothing: pure {vacation, discretionary, date} }

    slider onChangeCommitted = 
      DOM.div [Props.className "top-slider"] [MUI.slider (Option.fromRecord {onChangeCommitted, valueLabelDisplay: Auto})]



printStats :: ∀ a. UserInput -> Array Vacation -> Widget HTML a
printStats { vacationSoFar, discretionarySoFar, dateInTheFuture } vacations = do
  today <- liftEffect nowDate
  let
    thisMonth = MonthDate.fromDate today
    thisMonthStats = { vacationHours: Hours vacationSoFar, discretionaryHours: Hours discretionarySoFar }
    allStats = vacationStatsByMonth vacations thisMonthStats thisMonth dateInTheFuture

  DOM.table'
    [ DOM.thead' 
      [ DOM.tr'
        [ DOM.th' [DOM.text "Month"]
        , DOM.th' [DOM.text "Vacation Available"]
        , DOM.th' [DOM.text "Disc. Available"]
        , DOM.th' [DOM.text "Vacation Taken"]
        , DOM.th' [DOM.text "Holidays"]
        ]
      ]
    , DOM.tbody' 
      ( (Map.toUnfoldable allStats :: Array _) <#> 
        (\(monthDate@(MonthDate { month }) /\ vacation) -> 
          let
            vacationTaken = Array.filter (_.month >>> (==) monthDate) vacations # map (_.nHours) # fold # un Hours
            printNumber = show <<< Int.floor
          in DOM.tr' 
            [ DOM.td' [DOM.text $ prettyPrint monthDate]
            , DOM.td' [DOM.text $ i (printNumber $ un Hours vacation.vacationHours)" hrs"]
            , DOM.td' [DOM.text $ i (printNumber $ un Hours vacation.discretionaryHours)" hrs"]
            , DOM.td' [DOM.text $ i (printNumber vacationTaken)" hrs"]
            , DOM.td' [DOM.text $ show $ holidaysThisMonth month]
            ]
        )
      )
    ]


parseVacationTime :: ∀ m. MonadError String m => String -> m Vacation
parseVacationTime inputStr = liftError $ runParser inputStr do
  isNeg <- (try (string "-") $> true) <|> pure false
  absHours <- parseNumber <* skipSpaces
  try (string "hours" <|> string "hrs" <|> string "") *> skipSpaces
  try (string "in" <|> string "") *> skipSpaces
  month <- parseMonthDate
  let nHours = Hours $ if isNeg then negate absHours else absHours
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
    ( DOM.text "Any vacations to log? (e.g., 8 hours in May 2028)"
    <|> textInputWithButton "" "Enter" [Props._type "text", Props._id "log-button"] Option.empty
    )
  parseVacationTime ans

applyAnyVacations :: Widget HTML (Either String { nHours :: Hours, month :: MonthDate }) -> UserInput -> Widget HTML Unit
applyAnyVacations initialStats userInput = evalStateT (forever go) {stats: initialStats, currentVacations: []}
  where
  go = do
    {stats, currentVacations} <- get
    parseResults <- lift (stats <|> runExceptT getVacationTime)
    case parseResults of
      Left err -> 
        void 
          ( DOM.text err
          <|> MUI.button (Option.fromRecord {onClick: \x -> x}) [DOM.text "Try Again"]
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
