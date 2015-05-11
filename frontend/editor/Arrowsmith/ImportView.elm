module Arrowsmith.ImportView where

import Debug

import Char
import Color
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Graphics.Input.Field exposing (..)
import Signal exposing ((<~))
import String
import Text

import Arrowsmith.Module exposing (nameToString)
import Arrowsmith.Types exposing (Import, VarValue(..))

--
-- Model
--

type Action
  = NoOp
  | StartEditing
  | StopEditing
  | ChangeName Content

type State
  = ReadOnly { import_ : Import }
  | Editing { import_ : Import, nameContent : Content }

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

action : Action -> Signal.Message
action =
  Signal.message actions.address

step action state =
  case state of
    ReadOnly readState ->
      case action of
        NoOp -> state
        StartEditing ->
          Editing { import_ = readState.import_, nameContent = (content << nameToString) readState.import_.name }
        _ -> Debug.crash "Should not get into this state..."
    Editing editState ->
      case action of
        NoOp -> state
        StopEditing ->
          let
            oldImport = editState.import_
            newImport = { oldImport | name <- String.split "." editState.nameContent.string }
          in ReadOnly { import_ = newImport }
        ChangeName newContent ->
          Editing { editState | nameContent <- validate newContent }
        _ -> Debug.crash "Should not get into this state..."

state : Import -> Signal State
state import_ =
  Signal.foldp step (initialState import_) actions.signal

content : String -> Content
content str =
  { string = str, selection = Selection 0 0 Forward }

initialState : Import -> State
initialState i =
  ReadOnly { import_ = i }

validate : Content -> Content
validate content =
  let
    validateString =
      String.filter (\c -> (List.any (\f -> f c) [Char.isUpper, Char.isLower, (==) '.']))
  in
    { string = validateString content.string, selection = content.selection }


--
-- View
--

importView : Import -> Signal Element
importView import_ =
  scene <~ (state import_)

scene : State -> Element
scene state =
  flow right <| case state of
    ReadOnly {import_} ->
      let
        aliasPart = case import_.alias of
          Just alias -> [label "as", label alias]
          Nothing -> []
        exposingPart = [label "exposing"] ++ [label "..."]
      in
        [label (nameToString import_.name)] ++ aliasPart ++ exposingPart
    Editing {nameContent} ->
      [nameField nameContent, doneButton]

label str =
  Text.fromString str
    |> Text.style textStyle
    |> leftAligned
    |> color Color.white
    |> clickable (action StartEditing)

doneButton =
  Graphics.Input.button (action StopEditing) "&#10004;" --"✔"

nameField content =
  field fieldStyle (ChangeName >> action) "Module name" content

textStyle =
  { typeface = ["Roboto", "sans-serif"]
  , height = Just 16
  , color = Color.black
  , bold = False
  , italic = False
  , line = Nothing
  }

fieldStyle =
  { padding = uniformly 10
  , outline = noOutline
  , highlight = noHighlight
  , style = textStyle
  }

--
-- Testing
--

exampleImport : Import
exampleImport =
  { name = ["Bar", "Baz"]
  , alias = Just "BB"
  , exposedVars = { explicits = [VarValue "foo", VarValue "beb"], open = False }
  }

main =
  importView exampleImport
