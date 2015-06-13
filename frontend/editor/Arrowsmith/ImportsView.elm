module Arrowsmith.ImportsView (Action, Model, init, update, view) where

import Array exposing (Array)
import Array.Focus exposing (updateAt)
import Color exposing (Color)
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
    row (id, model) = H.div [ A.class "import-row" ] [ removeButton address id, importView address (id, model) ]
    rows = List.map row <| Array.toIndexedList importViews
    add =  H.div [ A.class "import-row" ] [ addButton address ]
  in
    H.div [ A.class "module-imports" ] <| rows ++ [ add ]

importView : Address Action -> (Int, ImportView.Model) -> Html
importView address (id, model) =
  ImportView.view (S.forwardTo address (Change id)) model

addButton : Address Action -> Html
addButton address =
  H.div [ A.class "import-add-button", E.onClick address Add ] [ fontAwesomeButton FontAwesome.plus ]

removeButton : Address Action -> Int -> Html
removeButton address id =
  H.div [ A.class "import-remove-button", E.onClick address (Remove id) ] [ fontAwesomeButton FontAwesome.times ]

fontAwesomeButton : (Color -> number -> Html) -> Html
fontAwesomeButton button =
  button (Color.rgb 33 33 33) 16

main : Signal Html
main =
  view actions.address <~ (model [])

emptyImport : Import
emptyImport =
  ([""], { alias = Nothing, exposedVars = { explicits = [], open = True } })

remove : Int -> Array a -> Array a
remove i xs =
  Array.append (Array.slice 0 i xs) (Array.slice (i + 1) (Array.length xs) xs)
