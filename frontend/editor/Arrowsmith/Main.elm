module Arrowsmith.Main where

import Debug

import Dict as D exposing (Dict)
import Graphics.Element exposing (Element, flow, down)
import Graphics.Input as Input
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import List
import Maybe
import Signal as S exposing (Signal, (<~))
import String

import Arrowsmith.Definition as Def
import Arrowsmith.Imports as Imports
import Arrowsmith.Module as Module
import Arrowsmith.Types exposing (..)

--
-- State & Actions
--

initialState : State
initialState =
  { modul = initialModule

  , isCompiling = False
  , compileStatus = Compiled
  , synced = True

  , editing = Nothing

  , values = D.empty
  , toEvaluate = Nothing

  , fresh = 0
  }

actions : S.Mailbox Action
actions =
  S.mailbox NoOp

--action : Action -> S.Message
--action =
--  S.message actions

step : Action -> State -> State
step action state =
  --Debug.log "state" <| case action of
  case (Debug.log "action" action) of
    NoOp ->
      state

    Edit name ->
      { state | editing <- Just name }
    StopEditing _ ->
      state -- Nothing happens, this is only used to call the JS "edit" function.
    FinishEditing (name, newBinding) ->
      { state
      | editing <- Nothing
      , modul <- Module.replaceDefinition state.modul name (name, Nothing, newBinding)
      }

    Evaluate e ->
      { state | toEvaluate <- Just e }
    FinishEvaluating (moduleName, name, value) ->
      { state
      | values <- D.insert name value state.values
      , toEvaluate <- Nothing
      }

    ModuleCompiled newModule ->
      { state
      | compileStatus <- Compiled
      , synced <- True
      , modul <- newModule
      }
    CompilationFailed error ->
      { state
      | compileStatus <- CompileError error
      , synced <- True
      }

-- State can be updated either by "in-house" actions or by events from the environment.
-- Port signals can just be merged into actions, but compilation requires a separate signal.
state : Signal State
state =
  let
    events = S.mergeMany [ actions.signal
                         , FinishEditing <~ editedValue
                         , FinishEvaluating <~ evaluatedValue
                         , ModuleCompiled <~ compiledModules
                         , CompilationFailed <~ compileErrors
                         ]
  in
    S.foldp step initialState events

--
-- Edit
--

-- Editing cycle:
-- Elm:StopEditing --stopEditing--> JS:get textfield value --editedValue--> Elm:FinishEditing

port editedValue : Signal (Name, ElmCode)

port stopEditing : Signal Name
port stopEditing =
  let
    extractValue a =
      case a of
        StopEditing v -> v
        _ -> ""
    isStopEditingAction a =
      case a of
        StopEditing _ -> True
        _ -> False
  in
    extractValue <~ S.filter isStopEditingAction NoOp actions.signal

--
-- Evaluate
--

-- Evaluation cycle:
-- Elm:Evaluate --evaluate--> JS:do evaluation --evaluatedValue--> Elm:FinishEvaluating

port evaluatedValue : Signal (ModuleName, Name, Value)

port evaluate : Signal (ModuleName, Name)
port evaluate =
  let
    extractValue a =
      case a of
        Evaluate v -> v
        _ -> ([], "")
    isEvaluateAction a =
      case a of
        Evaluate _ -> True
        _ -> False
  in
    extractValue <~ S.filter isEvaluateAction NoOp actions.signal

--
-- Compile
--

port initialModule : Module

port compiledModules : Signal Module

port compileErrors : Signal ElmError

--
-- Views
--

importView : ElmCode -> Html
importView code =
  div "import" [ H.text code ]

datatypeView : ElmCode -> Html
datatypeView code =
  div "datatype" [ H.code [] [ H.text code ] ]

codeView : Definition -> Html
codeView (name, tipe, binding) =
  let
    lineCount = List.length (String.lines binding)
  in
    editable name "textarea"
      [ A.class "definition-code", A.rows lineCount ]
      [ H.text binding ]

defHeaderView : ModuleName -> Definition -> Html
defHeaderView moduleName (name, tipe, _) =
  let
    nameTag = tag "definition-name" [] [ H.text name ]
    evalTag = tag "definition-evaluate" [ E.onClick actions.address (Evaluate (moduleName, name)) ] [ H.text "eval" ]
    header = case tipe of
      Just t ->
        [ nameTag, tag "definition-type" [] [ H.text t ], evalTag ]
      Nothing ->
        [ nameTag, evalTag ]
  in
    H.table [ A.class "definition-header" ]
      [ H.tr [] header ]

defView : ModuleName -> Definition -> Html
defView moduleName definition =
  let
    (name, tipe, binding) = definition
    class = "definition defname-" ++ name
  in
    H.div [ A.class class ] <|
      [ defHeaderView moduleName definition
      , codeView definition
      ]

newDefView : Html
newDefView =
  H.div [ A.class "definition new-definition" ] <|
    [ codeView Def.newDefinition ]

moduleView : Module -> Html
moduleView modul =
  let
    {name, imports, datatypes, defs, errors} = modul
  in
    div "module"
      [ div "module-header"
        [ H.span [ A.class "module-name" ] [ H.text <| Module.nameToString name ] ]
      , div "module-imports" <| List.map importView imports
      , div "module-adts" <| List.map datatypeView datatypes
      , div "module-defs" <| List.map (defView name) defs ++ [newDefView]
      , div "module-errors" <| List.map errorView errors
      ]

errorView : ElmError -> Html
errorView error =
  div "error" [ H.pre [] [ H.text error ] ]

view : State -> Html
view {modul, compileStatus} =
  div "modules" [ moduleView modul ]

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

button : String -> String -> Action -> Html
button className buttonText act =
  H.div
    [ A.class className
    , E.onClick actions.address act
    ]
    [ H.text buttonText ]

editable : Name -> String -> List H.Attribute -> List Html -> Html
editable name tagName additionalAttrs contents =
  let
    attrs = A.contenteditable True :: editActions name ++ additionalAttrs
  in
    H.node tagName attrs contents

editActions : Name -> List H.Attribute
editActions name =
  [ E.onFocus actions.address (Edit name), E.onBlur actions.address (StopEditing name) ]

visible : Bool -> H.Attribute
visible v =
  A.style [ ("visibility", if v then "visible" else "hidden") ]

div : String -> List Html -> Html
div class =
  H.div [ A.class class ]

tag : String -> List H.Attribute -> List Html -> Html
tag class attrs =
  H.td (A.class ("tag " ++ class) :: attrs)

--
-- Main
--

main : Signal Html
main =
  view <~ state
