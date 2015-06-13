module Arrowsmith.Views.FormView where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Arrowsmith.Types exposing (ViewInfo)


view : Signal Form -> Signal Element
view =
  Signal.map <| \form -> collage 400 400 [ form ]

info : ViewInfo
info =
  { matches = ((==) "Graphics.Collage.Form")
  , name = ["Arrowsmith", "Views", "FormView"]
  }
