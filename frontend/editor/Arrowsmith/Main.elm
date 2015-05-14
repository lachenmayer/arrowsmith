module Arrowsmith.Main where

import Html exposing (Html)
import Signal as S exposing (Signal, (<~))

import Arrowsmith.ModuleView as ModuleView
import Arrowsmith.Types exposing (..)

--
-- Ports
--

-- Editing cycle:
-- Elm:StopEditing --stopEditing--> JS:get textfield value --editedValue--> Elm:FinishEditing

port editedValue : Signal (VarName, ElmCode)

port stopEditing : Signal VarName
port stopEditing =
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

-- Evaluation cycle:
-- Elm:Evaluate --evaluate--> JS:do evaluation --evaluatedValue--> Elm:FinishEvaluating

port evaluatedValue : Signal (Name, VarName, String)

port evaluate : Signal (Name, List VarName)
port evaluate =
  let
    isEvaluateAction a =
      case a of
        ModuleView.Evaluate _ -> True
        _ -> False
    isModuleCompiledAction a =
      case a of
        ModuleView.ModuleCompiled _ -> True
        _ -> False
    evaluateActions (lastAction, _) =
      isEvaluateAction lastAction || isModuleCompiledAction lastAction
  in
    S.map snd <| S.filter evaluateActions (ModuleView.NoOp, ([], [])) <| S.map (\{moduleView} -> (moduleView.lastAction, (moduleView.modul.name, moduleView.toEvaluate))) model

port evaluateMain : Signal (Name)
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
  case action of
    NoOp -> model
    MainModule moduleAction ->
      { model | moduleView <- ModuleView.update moduleAction model.moduleView }

model =
  let
    events = S.mergeMany [ actions.signal
                         , MainModule << ModuleView.FinishEditing <~ editedValue
                         , MainModule << ModuleView.FinishEvaluating <~ evaluatedValue
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
