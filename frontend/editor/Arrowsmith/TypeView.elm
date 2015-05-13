module Arrowsmith.TypeView (typeView) where

import Html as H
import Html.Attributes as A

import Arrowsmith.Types exposing (..)

typeView : Type -> H.Html
typeView tipe =
  H.td [ A.class "tag definition-type" ] [ H.text "TODO" ]
