module MonthDate where

import Vacate.Prelude

import Data.Enum (Cardinality(..), cardinality)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

class PrettyPrint a where
  prettyPrint :: a -> String

newtype MonthDate = MonthDate { year :: Year, month :: Month }

derive newtype instance Eq MonthDate
derive newtype instance Bounded MonthDate
derive instance Newtype MonthDate _
derive instance Generic MonthDate _
instance Show MonthDate where
  show = genericShow

instance Ord MonthDate where
  compare (MonthDate a) (MonthDate b) = case compare a.year b.year of
    LT -> LT
    GT -> GT
    EQ -> compare a.month b.month

instance Enum MonthDate where
  succ (MonthDate { year, month })
    | Just nextMonth <- succ month = Just $ MonthDate { year, month: nextMonth }
    | Just nextYear <- succ year = Just $ MonthDate { year: nextYear, month: bottom }
    | otherwise = Nothing

  pred (MonthDate { year, month })
    | Just prevMonth <- pred month = Just $ MonthDate { year, month: prevMonth }
    | Just prevYear <- pred year = Just $ MonthDate { year: prevYear, month: top }
    | otherwise = Nothing

instance BoundedEnum MonthDate where
  cardinality = Cardinality (unwrap yearCardinality * unwrap monthCardinality)
    where
    yearCardinality = (cardinality :: Cardinality Year)
    monthCardinality = (cardinality :: Cardinality Month)

  fromEnum (MonthDate { year, month }) = fromEnum year * 12 + fromEnum month
  toEnum i = do
    year <- toEnum (i / 12)
    month <- toEnum (i `mod` 12)
    pure $ MonthDate { year, month }

instance PrettyPrint MonthDate where
  prettyPrint (MonthDate { year, month }) = i (show month) " " (show $ fromEnum year)

onTheFirst :: MonthDate -> Date
onTheFirst (MonthDate { year, month }) = canonicalDate year month $ clampEnum 1

fromDate :: Date -> MonthDate
fromDate d = MonthDate { year: year d, month: month d }

diff :: MonthDate -> MonthDate -> Int
diff (MonthDate l) (MonthDate r) = ((on (-) fromEnum) l.year r.year) * 12 + ((on (-) fromEnum) l.month r.month)
