module Arrowsmith.ImportsView (Action, Model, init, update, view) where

import Array exposing (Array)
import Array.Focus exposing (updateAt)
import Color
import FontAwesome
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Signal as S exposing (Signal, Mailbox, Address, (<~))

import Arrowsmith.ImportView as ImportView
import Arrowsmith.Types exposing (..)

type Action
  = NoOp
  | Add
  | Remove Int
  | Change Int ImportView.Action

type alias Model =
  { importViews : Array ImportView.Model }

init : List Import -> Model
init imports =
  { importViews = Array.fromList <| List.map ImportView.init imports }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Add ->
      { model
      | importViews <- Array.push { import_ = emptyImport, editing = True } model.importViews
      }
    Remove id ->
      { model
      | importViews <- remove id model.importViews
      }
    Change id action ->
      { model
      | importViews <- updateAt id (ImportView.update action) model.importViews
      }

model : List Import -> Signal Model
model imports =
  S.foldp update (init imports) actions.signal

view : Address Action -> Model -> Html
view address {importViews} =
  let
    row (id, model) = [ removeButton address id, importView address (id, model) ]
    rows = List.concatMap row <| Array.toIndexedList importViews
  in
    H.div [ A.class "module-imports" ] <| rows ++ [addButton address]

importView : Address Action -> (Int, ImportView.Model) -> Html
importView address (id, model) =
  ImportView.view (S.forwardTo address (Change id)) model

addButton : Address Action -> Html
addButton address =
  H.div [ E.onClick address Add ] [ FontAwesome.plus Color.white 16 ]

removeButton : Address Action -> Int -> Html
removeButton address id =
  H.div [ E.onClick address (Remove id) ] [ FontAwesome.times Color.white 16 ]

main : Signal Html
main =
  view actions.address <~ (model [])

emptyImport : Import
emptyImport =
  ([""], { alias = Nothing, exposedVars = { explicits = [], open = True } })

remove : Int -> Array a -> Array a
remove i xs =
  Array.append (Array.slice 0 i xs) (Array.slice (i + 1) (Array.length xs) xs)