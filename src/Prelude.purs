module Vacate.Prelude (module Prelude, module Exports, clampEnum) where

import Prelude

import Data.Date (Date, Month(..), Year, canonicalDate, month, year) as Exports
import Data.Enum (class BoundedEnum)
import Data.Enum (class Enum, fromEnum, toEnum, toEnumWithDefaults, succ, pred) as Exports
import Data.Foldable (sum) as Exports
import Data.Function (on) as Exports
import Data.Maybe (Maybe(..), fromJust) as Exports
import Data.Newtype (unwrap) as Exports

clampEnum :: ∀ a. BoundedEnum a => Int -> a 
clampEnum = Exports.toEnumWithDefaults bottom top

