module Arrowsmith.Views.ListView where

import Debug

import Graphics.Element exposing (show)
import Html as H exposing (Html)
import Html.Attributes as A
import Regex exposing (..)

import Arrowsmith.Types exposing (Type, ViewInfo)
import Arrowsmith.Util exposing (..)


view : Signal (List a) -> Signal Html
view =
  Signal.map <| \xs ->
    div "value-list" <|
      List.map (show >> \e -> div "list-element" [ H.fromElement e ]) xs

matches : Type -> Bool
matches tipe =
  contains (regex "^List .+") tipe && (not <| contains (regex "(.+?->.+?)+") tipe)

info : ViewInfo
info =
  { matches = matches
  , name = ["Arrowsmith", "Views", "ListView"]
  }
