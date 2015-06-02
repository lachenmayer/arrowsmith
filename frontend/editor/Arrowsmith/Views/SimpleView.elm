module Arrowsmith.Views.SimpleView where

import Graphics.Element exposing (..)

import Arrowsmith.Types exposing (ViewInfo)


view : a -> Element
view =
  show

info : ViewInfo
info =
  { matches = always True
  , name = ["Arrowsmith", "Views", "SimpleView"]
  }