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
import Arrowsmith.ModuleView as ModuleView
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
  | Structured ModuleView.Model

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
  | ModuleAction ModuleView.Action

  | ToggleEditorView
  | CompiledElmFile ElmFile


init : Model
init =
  let
    elmFile = convertElmFile initialElmFile
  in
    { elmFile = elmFile
    , isCompiling = False
    , compileStatus = Compiled
    , lastAction = NoOp
    , editorView = makeEditorView elmFile
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
          Structured m -> Structured <| ModuleView.update a m
          PlainText _ -> Debug.crash "Should never receive a structured action when in plain text mode!"
      }

    ToggleEditorView ->
      { model
      | lastAction <- ToggleEditorView
      , editorView <- case model.editorView of
          Structured m -> PlainText <| PlainTextView.init model.elmFile.source
          PlainText m -> makeEditorView model.elmFile
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
    events = S.mergeMany [ actions.signal
                         , ModuleAction << ModuleView.FinishEvaluating <~ finishEvaluating
                         , CompiledElmFile <~ compiledElmFiles
                         ]
  in
    S.foldp update init events

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
        ModuleAction (ModuleView.StopEditing v) -> v
        _ -> ""
    isStopEditingAction a =
      case a of
        ModuleAction (ModuleView.StopEditing _) -> True
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
        ModuleAction (ModuleView.Evaluate _ _) -> True
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
      <| S.filter isStructuredView init model

port evaluateMain : Signal (ModuleName)
port evaluateMain =
  S.map snd <| S.filter (fst >> (==) (ModuleAction ModuleView.EvaluateMain)) (NoOp, []) <| S.map (\{lastAction, elmFile} -> (lastAction, elmFile.fileName)) model

port initialElmFile : PortElmFile

port compiledElmFiles : Signal PortElmFile

convertElmFile : PortElmFile -> ElmFile
convertElmFile {filePath, fileName, source, compiledCode, modul, inRepo} =
  { filePath = filePath
  , fileName = fileName
  , source = source
  , compiledCode = compiledCode
  , modul = convertModule modul
  , inRepo = inRepo
  }

convertModule : PortModule -> Module
convertModule {name, imports, types, datatypes, aliases, defs, errors} =
  { name = name
  , imports = imports
  , types = List.map ...
  }

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
  in
    div ("module module-" ++ (String.join "-" name))
      [ div "module-header"
        [ H.span [ A.class "module-name" ] [ H.text <| Module.nameToString name ]
        , H.span [ A.class "module-buttons" ]
          [ H.span [ A.class "module-text-button", E.onClick address ToggleEditorView ] [ FontAwesome.file_text Color.white 24 ] ]
        ]
      , editorView address model
      ]

editorView : S.Address Action -> Model -> Html
editorView address {editorView} =
  case editorView of
    PlainText model -> PlainTextView.view (S.forwardTo address PlainTextAction) model
    Structured model -> ModuleView.view (S.forwardTo address ModuleAction) model

makeEditorView : ElmFile -> EditorView
makeEditorView elmFile =
  case elmFile.modul of
    Nothing -> PlainText <| PlainTextView.init elmFile.source
    Just modul -> Structured <| ModuleView.init modul

updateEditorView : EditorView -> ElmFile -> EditorView
updateEditorView editorView elmFile =
  case editorView of
    PlainText model -> makeEditorView elmFile
    Structured model -> case elmFile.modul of
      Nothing -> makeEditorView elmFile
      Just modul -> Structured <| ModuleView.update (ModuleView.ChangeModule modul) model

isStructuredView : Model -> Bool
isStructuredView {editorView} =
  case editorView of
    Structured _ -> True
    PlainText _ -> False

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

