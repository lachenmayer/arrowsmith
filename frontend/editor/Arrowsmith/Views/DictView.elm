module Arrowsmith.Views.DictView where

import Debug

import Dict exposing (Dict)
import Graphics.Element exposing (show)
import Html as H exposing (Html)
import Html.Attributes as A
import Regex exposing (..)

import Arrowsmith.Types exposing (Type, ViewInfo)
import Arrowsmith.Util exposing (..)

itemView : (comparable, a) -> Html
itemView (key, value) =
  div "dict-item"
    [ div "dict-key" [ H.fromElement (show key) ]
    , div "dict-value" [ H.fromElement (show value) ]
    ]

view : Dict comparable a -> Html
view xs =
  xs
    |> Dict.toList
    |> List.map itemView
    |> div "value-dict"

matches : Type -> Bool
matches tipe =
  contains (regex "^Dict\\.Dict .+") tipe && (not <| contains (regex "(.+?->.+?)+") tipe)

info : ViewInfo
info =
  { matches = matches
  , name = ["Arrowsmith", "Views", "DictView"]
  }
