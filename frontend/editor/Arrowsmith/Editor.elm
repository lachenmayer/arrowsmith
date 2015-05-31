module Arrowsmith.Editor where

import Debug

import Color
import FontAwesome
import Graphics.Element exposing (Element, flow, down)
import Graphics.Input as Input
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List
import Maybe
import Signal as S exposing (Signal, (<~))
import String

import Arrowsmith.Module as Module
import Arrowsmith.StructuredEditView as StructuredEditView exposing (Action(ChangeEditorView)) -- https://github.com/elm-lang/elm-compiler/issues/951
import Arrowsmith.PlainTextView as PlainTextView
import Arrowsmith.Types exposing (..)
import Arrowsmith.Util exposing (..)

-- Value views
-- Not used anywhere here, but called from JS.
import Arrowsmith.Views.ColorView
import Arrowsmith.Views.SimpleView


--
-- Model, actions, update, main
--

type EditorView
  = PlainText PlainTextView.Model
  | Structured StructuredEditView.Model

type alias Model =
  { elmFile : ElmFile
  , isCompiling : Bool
  , compileStatus : CompileStatus
  , lastAction : Action
  , editorView : EditorView
  }

type Action
  = NoOp

  | PlainTextAction PlainTextView.Action
  | ModuleAction StructuredEditView.Action

  | ToggleEditorView
  | CompiledElmFile ElmFile


init : ElmFile -> Model
init initialElmFile =
  { elmFile = initialElmFile
  , isCompiling = False
  , compileStatus = Compiled
  , lastAction = NoOp
  , editorView = makeEditorView initialElmFile
  }

actions : S.Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case Debug.log "Editor" action of
    NoOp ->
      { model
      | lastAction <- NoOp
      }

    ModuleAction ChangeEditorView ->
      { model
      | lastAction <- ModuleAction ChangeEditorView
      , editorView <- PlainText <| PlainTextView.init model.elmFile.source
      }

    PlainTextAction a ->
      { model
      | lastAction <- PlainTextAction a
      , editorView <- case model.editorView of
          PlainText m -> PlainText <| PlainTextView.update a m
          Structured _ -> Debug.crash "Should never receive a plain text action when in structured mode!"
      }
    ModuleAction a ->
      { model
      | lastAction <- ModuleAction a
      , editorView <- case model.editorView of
          Structured m -> Structured <| StructuredEditView.update a m
          PlainText _ -> Debug.crash "Should never receive a structured action when in plain text mode!"
      }

    CompiledElmFile newElmFile ->
      { model
      | lastAction <- CompiledElmFile newElmFile
      , elmFile <- newElmFile
      , editorView <- makeEditorView newElmFile
      }

model : Signal Model
model =
  let
    events = S.mergeMany
      [ actions.signal
      , ModuleAction << StructuredEditView.FinishEvaluating <~ finishEvaluating
      , CompiledElmFile <~ compiledElmFiles
      ]
  in
    S.foldp update (init initialElmFile) events

main : Signal Html
main =
  view actions.address <~ model

--
-- Ports
--

port editDefinition : Signal VarName
port editDefinition =
  let
    extractValue a =
      case a of
        ModuleAction (StructuredEditView.StopEditing v) -> v
        _ -> ""
    isStopEditingAction a =
      case a of
        ModuleAction (StructuredEditView.StopEditing _) -> True
        _ -> False
  in
    S.map extractValue
      <| S.filter isStopEditingAction NoOp actions.signal

port editText : Signal ()
port editText =
  S.map (\_ -> ())
    <| S.filter ((==) (PlainTextAction PlainTextView.StopEditing)) NoOp actions.signal

--port editImport : Signal Import
--port editImport =
--  S.filter importActions (NoOp, ()) <| S.map (\{moduleView} -> (moduleView.lastAction, moduleView.importsView)) model

port finishEvaluating : Signal (ModuleName, VarName, ModuleName)

port evaluate : Signal (ModuleName, List (VarName, List VarName))
port evaluate =
  let
    isEvaluateAction a =
      case a of
        ModuleAction (StructuredEditView.Evaluate _ _) -> True
        _ -> False
    isCompiledElmFileAction a =
      case a of
        CompiledElmFile _ -> True
        _ -> False
    evaluateActions (lastAction, _) =
      isEvaluateAction lastAction || isCompiledElmFileAction lastAction

    toEvaluate editorView =
      case editorView of
        Structured m -> m.toEvaluate
        PlainText _ -> []
  in
    S.map snd
      <| S.filter evaluateActions (NoOp, ([], []))
      <| S.map (\{lastAction, elmFile, editorView} -> (lastAction, (elmFile.fileName, toEvaluate editorView)))
      <| S.filter isStructuredView (init initialElmFile) model

port evaluateMain : Signal (ModuleName)
port evaluateMain =
  S.map snd <| S.filter (fst >> (==) (ModuleAction StructuredEditView.EvaluateMain)) (NoOp, []) <| S.map (\{lastAction, elmFile} -> (lastAction, elmFile.fileName)) model

port initialElmFile : ElmFile

port compiledElmFiles : Signal ElmFile

--
-- Views
--

view : S.Address Action -> Model -> Html
view address model =
  div "modules" [ moduleView address model ]

moduleView : S.Address Action -> Model -> Html
moduleView address model =
  let
    name = model.elmFile.fileName
    highlightErrors content =
      if model.elmFile.errors == [] then content else div "has-errors" [ content ]
  in
    div ("module module-" ++ (String.join "-" name))
      [ div "module-header"
        [ H.span [ A.class "module-name" ] [ H.text <| Module.nameToString name ] ]
      , errorView model.elmFile.errors
      , highlightErrors <| editorView address model
      ]

editorView : S.Address Action -> Model -> Html
editorView address {editorView} =
  case editorView of
    PlainText model -> PlainTextView.view (S.forwardTo address PlainTextAction) model
    Structured model -> StructuredEditView.view (S.forwardTo address ModuleAction) model

makeEditorView : ElmFile -> EditorView
makeEditorView elmFile =
  case elmFile.modul of
    Nothing -> PlainText <| PlainTextView.init elmFile.source
    Just modul -> Structured <| StructuredEditView.init modul

isStructuredView : Model -> Bool
isStructuredView {editorView} =
  case editorView of
    Structured _ -> True
    PlainText _ -> False

errorView : List ElmError -> Html
errorView errors =
  div "module-errors" <| List.map (\error -> div "error" [ H.pre [] [ H.text error ] ]) errors

--
-- Util
--

keepJust : Signal (Maybe a) -> a -> Signal a
keepJust maybes default =
  let
    decorate maybe =
      case maybe of
        Just x -> (True, x)
        Nothing -> (False, default)
  in
    snd <~ S.filter fst (False, default) (decorate <~ maybes)

