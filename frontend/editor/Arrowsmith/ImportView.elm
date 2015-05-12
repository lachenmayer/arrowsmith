module Arrowsmith.ImportView where

import Debug

import Char
import Color
import FontAwesome
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json
import Signal exposing ((<~))
import String
import Text

import Arrowsmith.Module exposing (nameToString)
import Arrowsmith.Types exposing (Import, Listing)

--
-- Model
--

type Action
  = NoOp
  | StartEditing
  | StopEditing
  | ChangeName String
  | ChangeAlias String
  | ChangeExposed String

type alias State =
  { import_ : Import, editing : Bool }

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

step : Action -> State -> State
step action state =
  case Debug.log "action" action of
    NoOp -> state
    StartEditing ->
      { state | editing <- True }
    StopEditing ->
      { state | editing <- False }
    ChangeName input ->
      let
        i = state.import_
        validatedName = validate input
      in { state | import_ <- { i | name <- String.split "." validatedName } }
    ChangeAlias input ->
      let
        i = state.import_
        validatedInput = validate input
        alias = if String.isEmpty validatedInput then Nothing else Just validatedInput
      in { state | import_ <- { i | alias <- alias } }
    --ChangeExposed input ->
    --  let
    --    i = state.import_
    --  in { state | import_ <- { i | exposedVars }}

state : Import -> Signal State
state import_ =
  Signal.foldp step (initialState import_) actions.signal

initialState : Import -> State
initialState i =
  { import_ = i, editing = False }

validate : String -> String
validate =
  String.filter (\c -> (List.any (\f -> f c) [Char.isUpper, Char.isLower, (==) '.']))

--listingToString : Listing String -> String
--listingToString listing =
--  "TODO"

--isEmptyListing : Listing a -> Bool
--isEmptyListing {explicits, open} =
--  not open && List.isEmpty explicits

--
-- View
--

importView : Import -> Signal H.Html
importView import_ =
  scene <~ (state import_)

scene : State -> H.Html
scene {editing, import_} =
  H.div [ A.class "import" ] <|
    nameField editing import_ ++ aliasField editing import_ ++ {-exposingField editing import_ ++-} doneButton editing

nameField : Bool -> Import -> List H.Html
nameField editing import_ =
  if editing then
    [ editable "import-name import-editing" (nameToString import_.name) ChangeName StopEditing ]
  else
    [ clickable "import-name" (nameToString import_.name) StartEditing ]

aliasField : Bool -> Import -> List H.Html
aliasField editing import_ =
  if editing then
    [ label "as", editable "import-alias import-editing" (Maybe.withDefault "" import_.alias) ChangeAlias StopEditing ]
  else
    case import_.alias of
      Just alias -> [ label "as", clickable "import-alias" alias StartEditing ]
      Nothing -> []

--exposingField editing import_ =
--  if editing then
--    [ label "exposing", editable "import-exposing import-editing" (listingToString import_.exposedVars) ChangeExposed StopEditing ]
--  else
--    if isEmptyListing import_.exposedVars then
--      []
--    else
--      [ label "exposing", clickable "import-exposing" (listingToString import_.exposedVars) StartEditing ]

doneButton editing =
  if editing then
    [ H.span [ A.class "import-done-button", E.onClick actions.address StopEditing ] [ FontAwesome.check Color.white 16 ] ]
  else
    []

label str =
  H.span [ A.class "import-label" ] [ H.text str ]

clickable className value clickAction =
  H.span [ A.class className, E.onClick actions.address clickAction ] [ H.text value ]

editable className value inputAction enterAction =
  H.input
    [ A.class className
    , A.value value
    , E.on "input" E.targetValue (inputAction >> Signal.message actions.address)
    , onEnter enterAction
    ]
    []

onEnter : Action -> H.Attribute
onEnter action =
  E.on "keydown"
    (Json.customDecoder E.keyCode (\code -> if code == 13 then Ok () else Err ""))
    (\_ -> Signal.message actions.address action)

--
-- Testing
--

--exampleImport : Import
--exampleImport =
--  { name = ["Bar", "Baz"]
--  , alias = Just "BB"
--  , exposedVars = { explicits = ["foo", "beb"], open = False }
--  }

--main =
--  importView exampleImport
