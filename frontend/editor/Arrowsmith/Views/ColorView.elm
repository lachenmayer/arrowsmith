module Arrowsmith.Views.ColorView where

import Color exposing (Color)
import Graphics.Element as E

view : Color -> Element
view color =
  E.color color (E.size 100 100 E.empty)
