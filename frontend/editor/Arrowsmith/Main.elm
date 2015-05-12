module Arrowsmith.Main where

import Html exposing (Html)
import Signal as S exposing (Signal, (<~))

import Arrowsmith.ImportView as ImportView
import Arrowsmith.ModuleView as ModuleView
import Arrowsmith.Types exposing (..)

--
-- Ports
--

-- Editing cycle:
-- Elm:StopEditing --stopEditing--> JS:get textfield value --editedValue--> Elm:FinishEditing

port editedValue : Signal (Name, ElmCode)

--port stopEditing : Signal Name
--port stopEditing =
--  let
--    extractValue a =
--      case a of
--        StopEditing v -> v
--        _ -> ""
--    isStopEditingAction a =
--      case a of
--        StopEditing _ -> True
--        _ -> False
--  in
--    extractValue <~ S.filter isStopEditingAction NoOp actions.signal

-- Evaluation cycle:
-- Elm:Evaluate --evaluate--> JS:do evaluation --evaluatedValue--> Elm:FinishEvaluating

port evaluatedValue : Signal (ModuleName, Name, Value)

--port evaluate : Signal (ModuleName, Name)
--port evaluate =
--  let
--    extractValue a =
--      case a of
--        Evaluate v -> v
--        _ -> ([], "")
--    isEvaluateAction a =
--      case a of
--        Evaluate _ -> True
--        _ -> False
--  in
--    extractValue <~ S.filter isEvaluateAction NoOp actions.signal

port initialModule : Module

port compiledModules : Signal Module

port compileErrors : Signal ElmError

type Action
  = NoOp
  | ModuleView ModuleView.Action

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
    ModuleView moduleAction ->
      { model | moduleView <- ModuleView.update moduleAction model.moduleView }

model =
  S.foldp update init actions.signal
  --let
  --  events = S.mergeMany [ actions.signal
  --                       , ModuleView << ModuleView.FinishEditing <~ editedValue
  --                       , ModuleView << ModuleView.FinishEvaluating <~ evaluatedValue
  --                       , ModuleView << ModuleView.ModuleCompiled <~ compiledModules
  --                       , ModuleView << ModuleView.CompilationFailed <~ compileErrors
  --                       ]
  --in

view : S.Address Action -> Model -> Html
view address model =
  ModuleView.view (S.forwardTo address ModuleView) model.moduleView

main : Signal Html
main =
  view actions.address <~ model
