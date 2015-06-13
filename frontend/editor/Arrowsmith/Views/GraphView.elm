module Arrowsmith.Views.GraphView where

import Debug

import Signal as S
import Graphics.Collage as C
import Graphics.Element as E exposing (Element)
import Text

import Arrowsmith.Types exposing (ViewInfo)

aggregate : Float -> (Float, Float, List Float) -> (Float, Float, List Float)
aggregate number (minimum, maximum, numbers) =
  (min number minimum, max number maximum, number :: numbers)

graph : (Float, Float, List Float) -> Element
graph (minimum, maximum, numbers) =
  let
    range = maximum - minimum
    width = 500
    height = 200
    padding = 50
    dy = height / range
    dx = width / toFloat (List.length numbers)
    lineCoordinates = List.indexedMap (\i n -> (width - toFloat i * dx, (n - minimum) * dy)) numbers
    --line = C.traced C.defaultLine (C.path [(10, 10), (50, 50)])
    line = C.traced C.defaultLine (C.path lineCoordinates)
    label n = C.toForm <| E.leftAligned <| Text.fromString <| toString n
  in
    C.collage (width + padding) (height + padding)
      [ C.move (negate (width / 2), negate (height / 2)) <| line
      , C.move (negate (width / 2) + 100, negate (height / 2)) <| label minimum
      , C.move (negate (width / 2) + 100, (height / 2)) <| label maximum
      ]

view : Signal Float -> Signal Element
view numbers =
  let infinity = 1 / 0
  in S.map graph (S.foldp aggregate (infinity, negate infinity, []) numbers)


info : ViewInfo
info =
  { matches = ((==) "Signal.Signal Float")
  , name = ["Arrowsmith", "Views", "GraphView"]
  }
