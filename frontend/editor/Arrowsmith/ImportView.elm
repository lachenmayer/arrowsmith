module Arrowsmith.ImportView (Action, Model, init, update, view) where

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

type alias Model =
  { import_ : Import, editing : Bool }

init : Import -> Model
init import_ =
  { import_ = import_, editing = False }

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case Debug.log "action" action of
    NoOp -> model
    StartEditing ->
      { model | editing <- True }
    StopEditing ->
      { model | editing <- False }
    ChangeName input ->
      let
        (_, importMethod) = model.import_
        validatedName = validate input
      in { model | import_ <- (String.split "." validatedName, importMethod) }
    ChangeAlias input ->
      let
        (name, importMethod) = model.import_
        validatedInput = validate input
        alias = if String.isEmpty validatedInput then Nothing else Just validatedInput
      in { model | import_ <- (name, { importMethod | alias <- alias }) }
    --ChangeExposed input ->
    --  let
    --    i = model.import_
    --  in { model | import_ <- { i | exposedVars }}

model : Import -> Signal Model
model import_ =
  Signal.foldp update (init import_) actions.signal

validate : String -> String
validate =
  String.filter (\c -> (List.any (\f -> f c) [Char.isUpper, Char.isLower, (==) '.']))

listingToString : Listing String -> String
listingToString {explicits, open} =
  let explicits' = if open then explicits ++ [".."] else explicits in
    "(" ++ String.join ", " explicits' ++ ")"


isEmptyListing : Listing a -> Bool
isEmptyListing {explicits, open} =
  not open && List.isEmpty explicits

--
-- View
--

importView : Import -> Signal H.Html
importView import_ =
  view <~ (model import_)

view : Model -> H.Html
view {editing, import_} =
  H.div [ A.class "import" ] <|
    nameField editing import_ ++ aliasField editing import_ ++ exposingField editing import_ ++ doneButton editing

nameField : Bool -> Import -> List H.Html
nameField editing (importName, _) =
  if editing then
    [ editable "import-name import-editing" (nameToString importName) ChangeName StopEditing ]
  else
    [ clickable "import-name" (nameToString importName) StartEditing ]

aliasField : Bool -> Import -> List H.Html
aliasField editing (_, importMethod) =
  if editing then
    [ label " as ", editable "import-alias import-editing" (Maybe.withDefault "" importMethod.alias) ChangeAlias StopEditing ]
  else
    case importMethod.alias of
      Just alias -> [ label " as ", clickable "import-alias" alias StartEditing ]
      Nothing -> []

exposingField editing (_, {exposedVars}) =
  if editing then
    [ label " exposing ", editable "import-exposing import-editing" (listingToString exposedVars) ChangeExposed StopEditing ]
  else
    if isEmptyListing exposedVars then
      []
    else
      [ label " exposing ", clickable "import-exposing" (listingToString exposedVars) StartEditing ]

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
