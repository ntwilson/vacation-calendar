module Vacate.Frontend.MUI where

import Vacate.Prelude

import Concur.Core (class LiftWidget, Widget)
import Concur.Core.DOM as Concur
import Concur.Core.Props (Props(..))
import Concur.React (HTML)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as Json
import Data.DateTime (DateTime(..))
import Data.JSDate as JSDate
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Option as Option
import React (Children, ReactClass, ReactElement, createElement, unsafeCreateElement, unsafeCreateLeafElement)
import React.Basic.DOM (CSS)
import React.SyntheticEvent (SyntheticMouseEvent)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import Vacate.Shared.MonthDate (MonthDate, onTheFirst)
import Vacate.Shared.MonthDate as MonthDate

class (ShiftMap (Widget HTML) m, MultiAlternative m, LiftWidget HTML m) <= ReactWidget m
instance (ShiftMap (Widget HTML) m, MultiAlternative m, LiftWidget HTML m) => ReactWidget m

el :: ∀ f props m a. ReactWidget m => Functor f => (f props -> HTML -> ReactElement) -> f (Props props a) -> Array (m a) -> m a
el reactClass = Concur.el' (\props children -> [reactClass props children])

elLeaf :: ∀ f props m a. ReactWidget m => Functor f => (f props -> ReactElement) -> f (Props props a) -> m a
elLeaf reactClass = Concur.elLeaf (\props -> [reactClass props])

optionToObject :: ∀ r a. Homogeneous r a => Option.Option r -> Object a
optionToObject = unsafeCoerce

-- We're taking a generic props type as input, probably an Option of some sort. As far as Concur is concerned though, it needs
-- some Functor of a prop type, and we need to convert that Option to a homogenous Functor. Typical usage would be to convert 
-- the Option to an Object, and just make each value of the Option into a `Props Foreign a`.
-- The view type (`v`) will be HTML, which is just an alias for an Array ReactElement.

-- Handlers are the tricky part of creating a `Props Foreign a`.  The Handler constructor is `Handler :: (a -> Effect Unit) -> p`.  
-- So in other words, you're proving a function that takes an `a -> Effect Unit` as input, and turns it into whatever your prop type is.
-- Here the prop type is Foreign, and `a` is some sort of SyntheticEvent (e.g., SyntheticMouseEvent)
-- , so `Handler :: (SyntheticMouseEvent -> Effect Unit) -> Foreign`
widget :: ∀ f m usedProps p a. ReactWidget m => Functor f =>
  (f p -> HTML -> ReactElement) -> (usedProps -> f (Props p a)) -> usedProps -> Array (m a) -> m a
widget constructor transform usedProps = el constructor (transform usedProps)


widgetLeaf :: ∀ f m usedProps p a. ReactWidget m => Functor f =>
  (f p -> ReactElement) -> (usedProps -> f (Props p a)) -> usedProps -> m a
widgetLeaf constructor transform usedProps = elLeaf constructor (transform usedProps)

makeHandler :: ∀ a event. (event -> a) -> Props Foreign a
makeHandler transformEvent = Handler (\effFn -> unsafeToForeign $ mkEffectFn1 \event -> effFn (transformEvent event))

type ButtonProps a = Option.Option
  ( className :: String
  , color :: String
  , disabled :: Boolean
  , id :: String
  , onClick :: SyntheticMouseEvent -> a
  , style :: CSS
  )

foreign import rawButton :: ReactClass {children :: Children}
rawButtonImpl :: Object Foreign -> Array ReactElement -> ReactElement
rawButtonImpl props = unsafeCreateElement rawButton $ unsafeCoerce props

button :: ∀ a m. ReactWidget m => ButtonProps a -> Array (m a) -> m a
button = widget rawButtonImpl makeProps
  where 
  makeProps props = optionToObject $ flip Option.modify' props
    { className: PrimProp <<< unsafeToForeign
    , color: PrimProp <<< unsafeToForeign
    , disabled: PrimProp <<< unsafeToForeign
    , id: PrimProp <<< unsafeToForeign
    , onClick: \fn -> makeHandler fn
    , style: PrimProp <<< unsafeToForeign
    }

data Orientation = Horizontal | Vertical
instance EncodeJson Orientation where
  encodeJson Horizontal = Json.fromString "horizontal"
  encodeJson Vertical = Json.fromString "vertical"

data SliderTrack = Inverted | Normal | NoTrack
instance EncodeJson SliderTrack where
  encodeJson Inverted = Json.fromString "inverted"
  encodeJson Normal = Json.fromString "normal"
  encodeJson NoTrack = Json.fromBoolean false

data ValueLabelDisplay = Auto | Off | On
instance EncodeJson ValueLabelDisplay where
  encodeJson Auto = Json.fromString "auto"
  encodeJson Off = Json.fromString "off"
  encodeJson On = Json.fromString "on"

type SliderProps a = Option.Option
  ( className :: String
  , color :: String
  , defaultValue :: Int
  , disabled :: Boolean
  , marks :: Boolean
  , max :: Int
  , min :: Int
  , onChange :: Int -> a
  , onChangeCommitted :: Int -> a
  , orientation :: Orientation
  , step :: Int
  , style :: CSS
  , track :: SliderTrack
  , value :: Int
  , valueLabelDisplay :: ValueLabelDisplay
  )

foreign import rawSlider :: ReactClass Void
rawSliderImpl :: Object Foreign -> ReactElement
rawSliderImpl props = unsafeCreateLeafElement rawSlider $ unsafeCoerce props

slider :: ∀ m a. ReactWidget m => SliderProps a -> m a
slider = widgetLeaf rawSliderImpl makeProps
  where 
  makeProps props = optionToObject $ flip Option.modify' props
    { className: PrimProp <<< unsafeToForeign
    , color: PrimProp <<< unsafeToForeign
    , defaultValue: PrimProp <<< unsafeToForeign
    , disabled: PrimProp <<< unsafeToForeign
    , marks: PrimProp <<< unsafeToForeign
    , max: PrimProp <<< unsafeToForeign
    , min: PrimProp <<< unsafeToForeign
    , onChange: \transformValue -> Handler (\effFn -> unsafeToForeign $ mkEffectFn2 \_event value -> effFn (transformValue value))
    , onChangeCommitted: \transformValue -> Handler (\effFn -> unsafeToForeign $ mkEffectFn2 \_event value -> effFn (transformValue value))
    , orientation: PrimProp <<< unsafeToForeign <<< encodeJson
    , step: PrimProp <<< unsafeToForeign
    , style: PrimProp <<< unsafeToForeign
    , track: PrimProp <<< unsafeToForeign <<< encodeJson
    , value: PrimProp <<< unsafeToForeign
    , valueLabelDisplay: PrimProp <<< unsafeToForeign <<< encodeJson
    }

data DatePickerView = Year | Month | Day
instance EncodeJson DatePickerView where
  encodeJson Year = Json.fromString "year"
  encodeJson Month = Json.fromString "month"
  encodeJson Day = Json.fromString "day"

type DatePickerProps a = Option.Option
  ( className :: String
  , views :: Array DatePickerView
  , label :: String
  , minDate :: Date
  , maxDate :: Date
  , value :: Date
  , onChange :: Date -> a
  )

foreign import data DateAdapter :: Type
foreign import datefnsAdapter :: DateAdapter
foreign import localizationProvider :: ReactClass {dateAdapter :: DateAdapter, children :: Children}
foreign import rawDatePicker :: ReactClass Void
foreign import rawTextField :: ReactClass Void
rawDatePickerImpl :: Object Foreign -> ReactElement
rawDatePickerImpl props =
  createElement localizationProvider {dateAdapter: datefnsAdapter} $ 
    [ unsafeCreateLeafElement rawDatePicker $ unsafeCoerce props ]

datePicker :: ∀ m a. ReactWidget m => DatePickerProps a -> m a
datePicker = widgetLeaf rawDatePickerImpl makeProps
  where 
  atMidnight date = DateTime date bottom
  makeProps = optionToObject <<< Option.insert' { renderInput: PrimProp $ unsafeToForeign $ unsafeCreateLeafElement rawTextField } <<< Option.modify'
    { className: PrimProp <<< unsafeToForeign
    , views: PrimProp <<< unsafeToForeign <<< map encodeJson 
    , label: PrimProp <<< unsafeToForeign
    , minDate: PrimProp <<< unsafeToForeign <<< JSDate.fromDateTime <<< atMidnight
    , maxDate: PrimProp <<< unsafeToForeign <<< JSDate.fromDateTime <<< atMidnight
    , value: PrimProp <<< unsafeToForeign <<< JSDate.fromDateTime <<< atMidnight
    , onChange: \transformValue -> Handler (\effFn -> unsafeToForeign $ mkEffectFn1 \value -> 
        case JSDate.toDate value of 
          Just d -> effFn (transformValue d)
          Nothing -> pure unit
      )
    }

type MonthDatePickerProps a = Option.Option
  ( className :: String
  , label :: String
  , minDate :: Date
  , maxDate :: Date
  , value :: MonthDate
  , onChange :: MonthDate -> a
  )

monthDatePicker :: ∀ m a. ReactWidget m => MonthDatePickerProps a -> m a
monthDatePicker = datePicker <<< Option.insert' { views: [Year, Month] } <<< Option.modify'
  { value: onTheFirst
  , onChange: \fn -> fn <<< MonthDate.fromDate
  }

foreign import rawDataGrid :: ReactClass Void
rawDataGridImpl :: Object Foreign -> ReactElement
rawDataGridImpl props = unsafeCreateLeafElement rawDataGrid $ unsafeCoerce props

type DataGridProps a r = Option.Option 
  ( classes :: Option.Option ( cell :: String, columnHeader :: String, columnHeaderTitle :: String )
  , columns :: Array (Option.Option ( field :: String, headerName :: String, width :: Int, sortable :: Boolean, flex :: Int ))
  , rows :: Array { id :: Int | r }
  , onRowClick :: { id :: Int, row :: { id :: Int | r } } -> a
  , autoHeight :: Boolean
  , hideFooter :: Boolean
  , selectionModel :: Array Int
  )

dataGrid :: ∀ m a r. ReactWidget m => DataGridProps a r -> m a
dataGrid = widgetLeaf rawDataGridImpl makeProps
  where 
  makeProps = optionToObject <<< Option.modify'
    { classes: PrimProp <<< unsafeToForeign
    , columns: PrimProp <<< unsafeToForeign
    , rows: PrimProp <<< unsafeToForeign
    , autoHeight: PrimProp <<< unsafeToForeign
    , hideFooter: PrimProp <<< unsafeToForeign
    , selectionModel: PrimProp <<< unsafeToForeign
    , onRowClick: \fn -> makeHandler fn
    }
