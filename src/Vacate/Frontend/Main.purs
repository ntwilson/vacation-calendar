module Vacate.Frontend.Main where

import Vacate.Prelude


content :: Widget HTML Unit
content = 
  text "Loading..."

main :: Effect Unit
main = runWidgetInDom "contents" content