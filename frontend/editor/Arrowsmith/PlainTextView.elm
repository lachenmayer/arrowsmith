module Arrowsmith.PlainTextView (Action(..), Model, init, update, view) where

import Color
import FontAwesome
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Signal as S exposing (Signal, Mailbox, Address, (<~))
import String

import Arrowsmith.Types exposing (..)
import Arrowsmith.Util exposing (..)

type Action
  = NoOp
  | StopEditing

type alias Model =
  { source : ElmCode }

init : ElmCode -> Model
init source =
  { source = source }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    StopEditing -> model

model : ElmCode -> Signal Model
model source =
  S.foldp update (init source) actions.signal

view : Address Action -> Model -> Html
view address {source} =
  let
    lineCount = List.length (String.lines source)
  in
    div "module-editor plaintext-editor"
      [ actionsView address
      , editable "textarea"
        [ A.class "module-code", A.rows lineCount ]
        [ H.text source ]
      ]

actionsView : Address Action -> Html
actionsView address =
  let
    button action icon =
      H.span [ A.class "action-button", E.onClick address action ] [ icon (Color.rgb 33 33 33) 24 ]
  in
    div "module-actions"
      [ div "action-buttons"
        [ button StopEditing FontAwesome.check ]
      ]
