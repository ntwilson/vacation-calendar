module Vacate.Test.Calculator where

import Vacate.Prelude

import Data.Time.Duration (Hours(..))
import MonthDate (MonthDate(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Vacate.Calculator (vacationAccrued)

spec :: Spec Unit
spec = describe "Calculator" do
  describe "vacationAccrued" do 
    it "calculates correctly" do
      vacationAccrued sept2021 sept2021 `shouldEqual` Hours  0.0
      vacationAccrued sept2021  dec2021 `shouldEqual` Hours 38.0
      vacationAccrued sept2021  mar2022 `shouldEqual` Hours 84.0

  where 
  sept2021 = MonthDate { year: clampEnum 2021, month: September }
  dec2021 = MonthDate { year: clampEnum 2021, month: December }
  mar2022 = MonthDate { year: clampEnum 2022, month: March }
