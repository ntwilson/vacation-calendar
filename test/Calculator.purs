module Vacate.Test.Calculator where

import Vacate.Prelude

import Data.Map as Map
import MonthDate (MonthDate(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Vacate.Calculator (vacationStatsByMonth)

spec :: Spec Unit
spec = describe "Calculator" do
  describe "vacationStatsByMonth" do 
    it "calculates correctly" do
      shouldEqual true true
      vacationStatsByMonth [] mempty sept2021 sept2021 `shouldEqual` (Map.fromFoldable [ (sept2021 /\ {vacationHours: Hours  0.0, discretionaryHours: Hours 0.0}) ])
      vacationStatsByMonth [{month: sept2021, nHours: Hours 4.0}] 
        {vacationHours: Hours 20.0, discretionaryHours: Hours 2.0} 
        sept2021 sept2021 
        `shouldEqual` (Map.fromFoldable [ (sept2021 /\ {vacationHours: Hours 18.0, discretionaryHours: Hours 0.0}) ])
      vacationStatsByMonth [] mempty sept2021  nov2021 `shouldEqual` (Map.fromFoldable 
        [ (sept2021 /\ {vacationHours: Hours 0.0, discretionaryHours: Hours 0.0} )
        , (oct2021 /\ {vacationHours: Hours 10.0, discretionaryHours: Hours 8.0} )
        , (nov2021 /\ {vacationHours: Hours 20.0, discretionaryHours: Hours 8.0} )
        ])
      vacationStatsByMonth [{month: dec2021, nHours: Hours 16.0}] 
        {vacationHours: Hours 20.0, discretionaryHours: Hours 24.0} 
        nov2021 jan2022 
        `shouldEqual` (Map.fromFoldable 
          [ (nov2021 /\ {vacationHours: Hours 20.0, discretionaryHours: Hours 24.0})
          , (dec2021 /\ {vacationHours: Hours 30.0, discretionaryHours: Hours 8.0})
          , (jan2022 /\ {vacationHours: Hours 40.0, discretionaryHours: Hours 0.0})
          ])

  where 
  sept2021 = MonthDate { year: clampEnum 2021, month: September }
  oct2021 = MonthDate { year: clampEnum 2021, month: October }
  nov2021 = MonthDate { year: clampEnum 2021, month: November }
  dec2021 = MonthDate { year: clampEnum 2021, month: December }
  jan2022 = MonthDate { year: clampEnum 2022, month: January }
