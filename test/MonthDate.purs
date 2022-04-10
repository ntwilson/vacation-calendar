module Vacate.Test.MonthDate where

import Vacate.Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Vacate.Shared.MonthDate (MonthDate(..), diff)

spec :: Spec Unit
spec = describe "MonthDate" do
  describe "diff" do
    it "works within the same year" do
      (sept2021 `diff` june2021) `shouldEqual` 3
    it "works across a year boundary" do 
      (june2021 `diff` sept2020) `shouldEqual` 9
    it "works with negative diffs" do 
      (june2021 `diff` sept2021) `shouldEqual` (-3)
      (sept2020 `diff` june2021) `shouldEqual` (-9)

  describe "Enum" do 
    it "can round trip a MonthDate" do 
      (toEnum $ fromEnum $ sept2021) `shouldEqual` (Just sept2021)

    it "can count by MonthDate" do 
      (enumFromTo sept2020 june2021) `shouldEqual` sept2020ToJune2021

  where 
  yr2021 = clampEnum 2021
  yr2020 = clampEnum 2020

  june2021 = MonthDate { year: yr2021, month: June }
  sept2021 = MonthDate { year: yr2021, month: September }
  sept2020 = MonthDate { year: yr2020, month: September }

  sept2020ToJune2021 :: Array MonthDate 
  sept2020ToJune2021 = 
    ((enumFromTo September December) <#> (\month -> MonthDate { year: yr2020, month }))
    <> 
    ((enumFromTo January June) <#> (\month -> MonthDate { year: yr2021, month }))

