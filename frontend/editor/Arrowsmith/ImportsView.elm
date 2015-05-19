module Arrowsmith.ImportsView (Action, Model, init, update, view) where

import Html as H exposing (Html)
import Html.Attributes as A
import Signal as S exposing (Signal, Mailbox, Address, (<~))

import Arrowsmith.ImportView as ImportView
import Arrowsmith.Types exposing (..)

type Action
  = NoOp
  | Add Import
  | Remove Int
  | Change Int ImportView.Action

type alias Model =
  { importViews : List ImportView.Model }

init : List Import -> Model
init imports =
  { importViews = List.map ImportView.init imports }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

model : List Import -> Signal Model
model imports =
  S.foldp update (init imports) actions.signal

view : Address Action -> Model -> Html
view address model =
  H.div [ A.class "module-imports" ] [ H.text "yeah buddy" ]

main : Signal Html
main =
  view actions.address <~ (model [])
