module Arrowsmith.Views.SimpleView where

import Graphics.Element exposing (..)

import Arrowsmith.Types exposing (ViewInfo)


view : Signal a -> Signal Element
view =
  Signal.map show

info : ViewInfo
info =
  { matches = always True
  , name = ["Arrowsmith", "Views", "SimpleView"]
  }
