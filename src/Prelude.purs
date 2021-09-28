module Vacate.Prelude (module Prelude, module Exports, clampEnum) where

import Prelude

import Data.Date (Date, Month(..), Year, canonicalDate, month, year) as Exports
import Data.Either (Either(..), note) as Exports
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum, enumFromTo, toEnumWithDefaults, succ, pred) as Exports
import Data.Foldable (sum, fold) as Exports
import Data.Function (on) as Exports
import Data.Interpolate (i) as Exports
import Data.Maybe (Maybe(..), fromJust, fromMaybe) as Exports
import Data.Newtype (class Newtype, un, unwrap) as Exports
import Effect (Effect) as Exports
import Effect.Aff (launchAff_) as Exports
import Effect.Class (liftEffect) as Exports
import Effect.Class.Console (log) as Exports

clampEnum :: ∀ a. Exports.BoundedEnum a => Int -> a 
clampEnum = Exports.toEnumWithDefaults bottom top

