module Arrowsmith.Views.ColorView where

import Color exposing (Color)
import Graphics.Element as E exposing (Element)

import Arrowsmith.Types exposing (ViewInfo)


view : Signal Color -> Signal Element
view =
  Signal.map <| \color -> E.color color (E.size 100 100 E.empty)

info : ViewInfo
info =
  { matches = ((==) "Color.Color")
  , name = ["Arrowsmith", "Views", "ColorView"]
  }
