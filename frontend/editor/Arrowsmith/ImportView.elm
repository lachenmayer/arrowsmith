module Arrowsmith.ImportView where

import Debug

import Color
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Graphics.Input.Field exposing (..)
import Signal exposing ((<~))
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
        StartEditing -> Editing { import_ = readState.import_, nameContent = (content << nameToString) readState.import_.name }
        _ -> Debug.crash "Should not get into this state..."
    Editing editState ->
      case action of
        NoOp -> state
        StopEditing -> ReadOnly { import_ = editState.import_ }
        ChangeName newContent -> Editing { editState | nameContent <- newContent }
        _ -> Debug.crash "Should not get into this state..."

state : Import -> Signal State
state import_ =
  Signal.foldp step (initialState import_) actions.signal

content : String -> Content
content str =
  { string = str, selection = Selection 0 0 Forward }

initialState i =
  ReadOnly { import_ = i }

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
      [nameReadOnly import_.name]
    Editing {import_, nameContent} ->
      [nameField nameContent]

nameReadOnly name =
  Text.fromString (nameToString name)
    |> Text.style textStyle
    |> leftAligned
    |> color Color.white
    |> clickable (action StartEditing)

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
