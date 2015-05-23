module Arrowsmith.Main where

import Debug

import Html exposing (Html)
import Signal as S exposing (Signal, (<~))

import Arrowsmith.ModuleView as ModuleView
import Arrowsmith.Types exposing (..)

-- Views
-- Not used anywhere here, but called from JS.
import Arrowsmith.Views.SimpleView

--
-- Ports
--

port editedValue : Signal (VarName, ElmCode)

port editDefinition : Signal VarName
port editDefinition =
  let
    extractValue a =
      case a of
        MainModule (ModuleView.StopEditing v) -> v
        _ -> ""
    isStopEditingAction a =
      case a of
        MainModule (ModuleView.StopEditing _) -> True
        _ -> False
  in
    extractValue <~ S.filter isStopEditingAction NoOp actions.signal

--port editImport : Signal Import
--port editImport =
--  S.filter importActions (ModuleView.NoOp, ()) <| S.map (\{moduleView} -> (moduleView.lastAction, moduleView.importsView)) model

-- Evaluation cycle:
-- Elm:Evaluate --evaluate--> JS:do evaluation --evaluatedValue--> Elm:FinishEvaluating

port finishEvaluating : Signal (ModuleName, VarName, ModuleName)

port evaluate : Signal (ModuleName, List (VarName, ModuleName))
port evaluate =
  let
    isEvaluateAction a =
      case a of
        ModuleView.Evaluate _ _ -> True
        _ -> False
    isModuleCompiledAction a =
      case a of
        ModuleView.ModuleCompiled _ -> True
        _ -> False
    evaluateActions (lastAction, _) =
      isEvaluateAction lastAction || isModuleCompiledAction lastAction
  in
    S.map snd <| S.filter evaluateActions (ModuleView.NoOp, ([], [])) <| S.map (\{moduleView} -> (moduleView.lastAction, (moduleView.modul.name, moduleView.toEvaluate))) model

port evaluateMain : Signal (ModuleName)
port evaluateMain =
  S.map snd <| S.filter (fst >> (==) ModuleView.EvaluateMain) (ModuleView.NoOp, []) <| S.map (\{moduleView} -> (moduleView.lastAction, moduleView.modul.name)) model

port initialModule : Module

port compiledModules : Signal Module

port compileErrors : Signal ElmError

type Action
  = NoOp
  | MainModule ModuleView.Action

type alias Model =
  { moduleView : ModuleView.Model }

init : Model
init =
  { moduleView = ModuleView.init initialModule }

actions : S.Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case (Debug.log "action" action) of
    NoOp -> model
    MainModule moduleAction ->
      { model | moduleView <- ModuleView.update moduleAction model.moduleView }

model =
  let
    events = S.mergeMany [ actions.signal
                         , MainModule << ModuleView.FinishEditing <~ editedValue
                         , MainModule << ModuleView.FinishEvaluating <~ finishEvaluating
                         , MainModule << ModuleView.ModuleCompiled <~ compiledModules
                         , MainModule << ModuleView.CompilationFailed <~ compileErrors
                         ]
  in
    S.foldp update init events

view : S.Address Action -> Model -> Html
view address model =
  ModuleView.view (S.forwardTo address MainModule) model.moduleView

main : Signal Html
main =
  view actions.address <~ model
