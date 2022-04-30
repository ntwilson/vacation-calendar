module Vacate.Frontend.Main where

import Vacate.Frontend.Prelude

import Concur.React.DOM as DOM
import Concur.React.Props as Props
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Effect.Now (nowDate)
import Option as Option
import React.Basic.DOM (css)
import Vacate.Frontend.MUI (ValueLabelDisplay(..), dataGrid, monthDatePicker)
import Vacate.Frontend.MUI as MUI
import Vacate.Shared.Calculator (holidaysThisMonth, vacationStatsByMonth)
import Vacate.Shared.MonthDate (MonthDate(..), prettyPrint)
import Vacate.Shared.MonthDate as MonthDate

type UserInput = { vacationSoFar :: Number, discretionarySoFar :: Number, dateInTheFuture :: MonthDate }

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
      DOM.div [Props.className "top-slider"] [MUI.slider (Option.fromRecord {onChangeCommitted, valueLabelDisplay: On})]

type TableRow = { id :: Int, month :: String, vacation :: String, discretionary :: String, taken :: String, holidays :: String, monthValue :: MonthDate }
data TableInteraction = RowSelection { id :: Int, row :: TableRow } | AdjustVacation Int

vacationTable :: ∀ a. UserInput -> Widget HTML a
vacationTable { vacationSoFar, discretionarySoFar, dateInTheFuture } = go {vacations: [], selectedRow: Nothing}
  where
  go {vacations, selectedRow} = do
    today <- liftEffect nowDate
    let
      thisMonth = MonthDate.fromDate today
      thisMonthStats = { vacationHours: Hours vacationSoFar, discretionaryHours: Hours discretionarySoFar }
      allStats = vacationStatsByMonth vacations thisMonthStats thisMonth dateInTheFuture

    interaction <- 
      ( ( dataGrid $ Option.fromRecord
          { classes: Option.fromRecord { cell: "wrapping-cell", columnHeaderTitle: "wrapping-header" }
          , columns:
            [ Option.fromRecord { field: "month", headerName: "Month", sortable: false, flex: 1 }
            , Option.fromRecord { field: "vacation", headerName: "Vacation Available", sortable: false, flex: 1 }  
            , Option.fromRecord { field: "discretionary", headerName: "Disc. Available", sortable: false, flex: 1 }
            , Option.fromRecord { field: "taken", headerName: "Vacation Taken", sortable: false, flex: 1 }
            , Option.fromRecord { field: "holidays", headerName: "Holidays", sortable: false, flex: 1 }
            ]
          , rows: case selectedRow of 
            Just {row} -> [ row ]
            Nothing -> 
              ( (Map.toUnfoldable allStats :: Array _) # Array.mapWithIndex 
                (\id (monthDate@(MonthDate { month }) /\ vacation) -> 
                  let
                    vacationTaken = Array.filter (_.month >>> (_ == monthDate)) vacations # map (_.nHours) # fold # un Hours
                    printNumber = show <<< Int.floor
                  in
                    { id
                    , month: prettyPrint monthDate
                    , monthValue: monthDate
                    , vacation: i (printNumber $ un Hours vacation.vacationHours)" hrs" :: String
                    , discretionary: i (printNumber $ un Hours vacation.discretionaryHours)" hrs" :: String
                    , taken: i (printNumber vacationTaken)" hrs" :: String
                    , holidays: show $ holidaysThisMonth month
                    }
                )
              )
          , onRowClick: \{id, row} -> RowSelection {id, row}
          , autoHeight: true
          , hideFooter: true
          , selectionModel: selectedRow # caseMaybe { just: \{id} -> [id], nothing: [] }
          }
        )
      <|> case selectedRow of
        Just {id, row} -> 
          ( DOM.div' [DOM.text $ i "How much vacation will you take in "row.month"?"]
          <|> DOM.div [Props.className "top-slider"] 
            [ MUI.slider $ Option.fromRecord 
              { value: 
                vacations 
                # Array.find (\{month} -> prettyPrint month == row.month)
                # caseMaybe {just: Int.floor <<< un Hours <<< _.nHours, nothing: 0}
              , onChange: AdjustVacation
              , valueLabelDisplay: On
              }
            ] 
          <|> MUI.button (Option.fromRecord {onClick: const $ RowSelection {id, row}, style: css {display: "flex"}}) [DOM.text "Enter"]
          )
        Nothing -> mempty
      )

    let 
      selection = case interaction, selectedRow of
        RowSelection newSelectedRow, Just {id} | newSelectedRow.id == id -> Nothing
        RowSelection newSelectedRow, _ -> Just newSelectedRow
        _, _ -> selectedRow

      newVacations = case interaction, selectedRow of
        AdjustVacation hrs, Just {row} -> case Array.find (\{month} -> prettyPrint month == row.month) vacations of
          Just vacation -> vacations # Array.delete vacation # Array.cons {month: vacation.month, nHours: Hours (Int.toNumber hrs)}
          Nothing -> vacations # Array.cons {month: row.monthValue, nHours: Hours (Int.toNumber hrs)}
        _, _ -> vacations
      

    go {vacations: newVacations, selectedRow: selection}

content :: Widget HTML Unit
content = do
  input <- getInput
  vacationTable input 


main :: Effect Unit
main = runWidgetInDom "contents" content
