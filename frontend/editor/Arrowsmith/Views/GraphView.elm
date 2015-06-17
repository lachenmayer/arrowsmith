module Arrowsmith.Views.GraphView where

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
    line = C.traced C.defaultLine (C.path lineCoordinates)
    label n = C.toForm <| E.leftAligned <| Text.fromString <| toString n
    topEdge = height / 2
    bottomEdge = negate (height / 2)
    leftEdge = negate (width / 2)
  in
    C.collage (width + padding) (height + padding)
      [ C.move (leftEdge, bottomEdge) <| line
      , C.move (leftEdge + 100, bottomEdge) <| label minimum
      , C.move (leftEdge + 100, topEdge) <| label maximum
      ]

view : Signal Float -> Signal Element
view =
  let
    infinity = 1 / 0
  in
    S.map graph << S.foldp aggregate (infinity, negate infinity, [])

info : ViewInfo
info =
  { matches = ((==) "Signal.Signal Float")
  , name = ["Arrowsmith", "Views", "GraphView"]
  }
