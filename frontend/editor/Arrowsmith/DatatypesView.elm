module Arrowsmith.DatatypesView (Action, Model, init, update, view) where

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as A
import Signal as S exposing (Signal, Mailbox, Address, (<~))

import Arrowsmith.Types exposing (..)

type Action
  = NoOp

type alias Model =
  { datatypes : List (VarName, AdtInfo) }

init : List (VarName, AdtInfo) -> Model
init adts =
  { datatypes = adts }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

model : List (VarName, AdtInfo) -> Signal Model
model adts =
  S.foldp update (init adts) actions.signal

view : Address Action -> Model -> Html
view address {datatypes} =
  H.div [ A.class "module-datatypes" ] <| List.map datatypeView datatypes

datatypeView : (VarName, AdtInfo) -> Html
datatypeView (name, {adtVars, constructors}) =
  H.table [ A.class "datatype" ]
    [ H.tr []
      [ H.td [ A.class "datatype-lhs" ] [ datatypeLhs name adtVars ]
      , H.td [ A.class "datatype-constructors" ] (datatypeConstructors constructors)
      ]
    ]

datatypeLhs : VarName -> List VarName -> Html
datatypeLhs name vars =
  H.div [] <| [ span "datatype-name" name ] ++ List.map (span "datatype-var") vars

datatypeConstructors : List (String, List Type) -> List Html
datatypeConstructors constructors =
  let
    constructor (name, types) =
      H.div [ A.class "datatype-constructor" ] <|
        [ span "datatype-constructor-name" name ] ++ List.map (span "datatype-constructor-type") types
  in
    List.map constructor constructors

span : String -> String -> Html
span class content =
  H.span [ A.class class ] [ H.text content ]

main : Signal Html
main =
  view actions.address <~ (model [])
