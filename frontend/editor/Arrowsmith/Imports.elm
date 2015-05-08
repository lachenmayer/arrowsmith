module Arrowsmith.Imports where

import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Signal exposing ((<~))

import Arrowsmith.Types exposing (Import, VarValue(..))

type Action
  = NoOp
  | StartEditing
  | StopEditing

type alias State =
  { import_ : Import
  , editing : Bool
  }

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

exampleImport : Import
exampleImport =
  { name = ["Bar", "Baz"]
  , alias = Just "BB"
  , exposedVars = { explicits = [VarValue "foo", VarValue "beb"], open = False }
  }

initialState i =
  { import_ = i
  , editing = True
  }

step action state =
  case action of
    NoOp -> state
    StartEditing -> { state | editing <- True }
    StopEditing -> { state | editing <- False }

scene nameMessage nameContent =
  flow right [nameField nameMessage nameContent]

importName : String -> Signal.Mailbox Content
importName content =
  Signal.mailbox { string = content, selection = Selection 0 0 Forward }

nameField nameMessage nameContent =
  field defaultStyle nameMessage "Module name" nameContent

--field defaultStyle (Signal.message mailbox.address) "Module name" <~ mailbox.signal

state : State -> Signal State
state initial =
  Signal.foldp step initial actions.signal


main =
  nameField (importName "Tweets")
