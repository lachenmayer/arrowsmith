module Arrowsmith.Views.SignalView where

import Graphics.Element exposing (..)
import Regex exposing (..)

import Arrowsmith.Types exposing (Type, ViewInfo)


view : Signal a -> Signal Element
view = Signal.map show

matches : Type -> Bool
matches tipe =
  contains (regex "^Signal ") tipe && (not <| contains (regex "(.+?->.+?)*") tipe)

info : ViewInfo
info =
  { matches = matches
  , name = ["Arrowsmith", "Views", "SignalView"]
  }