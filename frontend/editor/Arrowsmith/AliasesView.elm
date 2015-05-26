module Arrowsmith.AliasesView (Action, Model, init, update, view) where

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Signal as S exposing (Signal, Mailbox, Address, (<~))

import Arrowsmith.Types exposing (..)

type Action
  = NoOp

type alias Model =
  { aliases : List (VarName, Alias) }

init : List (VarName, Alias) -> Model
init a =
  { aliases = a }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

model : List (VarName, Alias) -> Signal Model
model a =
  S.foldp update (init a) actions.signal

view : Address Action -> Model -> Html
view address {aliases} =
  H.div [ A.class "module-aliases" ] <| List.map aliasView aliases

aliasView : (VarName, Alias) -> Html
aliasView (name, (vars, tipe)) =
  H.div [ A.class "module-alias" ] <| [ span "alias-name" name ] ++ List.map (span "alias-var") vars ++ [ span "alias-type" tipe ]

span : String -> String -> Html
span class content =
  H.span [ A.class class ] [ H.text content ]

main : Signal Html
main =
  view actions.address <~ (model [])
