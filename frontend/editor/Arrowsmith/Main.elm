module Arrowsmith.Main where

import Debug

import Dict (Dict)
import Dict
import Graphics.Element (Element, flow, down)
import Graphics.Input as Input
import Html (Html)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import List
import Maybe
import Signal (Signal, (<~))
import Signal as S
import Text (plainText)

import Arrowsmith.Definition as Def
import Arrowsmith.Module as Module
import Arrowsmith.Types (..)

--
-- State & Actions
--

type alias State =
  { program : Module

  , isCompiling : Bool
  , compilationStatus : CompilationStatus
  , dirty : Bool -- The code has changed (eg. by editing), but it has not been recompiled yet.

  , editing : Maybe Name
  
  , values : Dict Name Value
  , toEvaluate : Maybe (ModuleName, Name)

  , fresh : Int -- Used to generate fresh variable names.
  }

type Action
  = NoOp

  | Compile Module
  | FinishCompiling (CompileResponse, String)

  | Edit Name
  | StopEditing
  | FinishEditing (Name, Value)

  | Evaluate (ModuleName, Name)
  | FinishEvaluating (ModuleName, Name, Value)

  | NewDefinition
  | RemoveDefinition Name

initialState : State
initialState =
  { program = Module.helloWorld

  , isCompiling = False
  , compilationStatus = Ok ""
  , dirty = True

  , editing = Nothing

  , values = Dict.empty
  , toEvaluate = Nothing

  , fresh = 0
  }

actions : S.Channel Action
actions =
  S.channel NoOp

action : Action -> S.Message
action =
  S.send actions

step : Action -> State -> State
step action state =
  case (Debug.log "action" action) of
    NoOp ->
      state

    Compile m ->
      { state | isCompiling <- True }
    FinishCompiling (response, codeOrError) ->
      { state | isCompiling <- False
              , dirty <- False
              , compilationStatus <- case response of
                  "Ok" -> Ok codeOrError
                  "Err" -> Err codeOrError }

    Edit name ->
      { state | editing <- Just name }
    StopEditing ->
      state -- Nothing happens, this is only used to call the JS "edit" function.
    FinishEditing (newName, newBinding) ->
      case state.editing of
        Just oldName ->
          { state 
          | editing <- Nothing
          , dirty <- True
          , program <- Module.replaceDefinition state.program oldName { name = newName, tipe = Nothing, binding = newBinding }
          }
        Nothing -> Debug.crash "FinishEditing should never happen if not editing something!"

    Evaluate e ->
      { state | toEvaluate <- Just e }
    FinishEvaluating (moduleName, name, value) ->
      { state | values <- Dict.insert name value state.values
              , toEvaluate <- Nothing }

    NewDefinition ->
      { state | program <- Module.freshDefinition state.program state.fresh
              , fresh <- state.fresh + 1 }

    RemoveDefinition name ->
      { state | program <- Module.removeDefinition state.program name
              , dirty <- True }

-- State can be updated either by "in-house" actions or by events from the environment.
-- Port signals can just be merged into actions, but compilation requires a separate signal.
state : Signal State
state =
  let
    events = S.mergeMany [ S.subscribe actions
                         , FinishCompiling <~ compileResponse
                         , FinishEditing <~ editorValue
                         , FinishEvaluating <~ evaluatedValue 
                         ]
  in
    S.foldp step initialState events

--
-- Edit
--

-- Editing cycle:
-- Elm:StopEditing --stopEditing--> JS:get textfield value --editorValue--> Elm:FinishEditing

port editorValue : Signal (Name, Value)

port stopEditing : Signal ()
port stopEditing =
  always () <~ S.keepIf ((==) StopEditing) NoOp (S.subscribe actions)

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
    extractValue <~ S.keepIf isEvaluateAction NoOp (S.subscribe actions)

--
-- Compile
--

port compileResponse : Signal (CompileResponse, String)

port compileProgram : Signal ElmCode
port compileProgram =
  let
    extractValue a =
      case a of
        Compile modul -> modul
        _ -> Module.empty
    isCompileAction a =
      case a of
        Compile _ -> True
        _ -> False
    compileAction = extractValue <~ S.keepIf isCompileAction NoOp (S.subscribe actions)
    dirtyState = .program <~ S.keepIf .dirty initialState state
  in
    Module.toString <~ S.merge compileAction dirtyState

--
-- Views
--

importView : ElmCode -> Html
importView code =
  div "import" [ H.text code ]

adtView : ElmCode -> Html
adtView code =
  div "adt" [ H.code [] [ H.text code ] ]

editEvent : Bool -> Name -> H.Attribute
editEvent isEditing name =
  if isEditing then
    E.onBlur <| action StopEditing
  else
    E.onClick <| action (Edit name)


codeView : Bool -> Definition -> Html
codeView isEditing {name, binding} =
  let
    content =
      if (binding == Module.undefinedBinding) || (binding == "") then
        H.span [ A.class "undefined-definition" ] [ H.text (if isEditing then "" else "undefined") ]
      else
        H.text binding
  in
    editable isEditing name "div"
      [ A.class "definition-code" ]
      [ content ]

defHeaderView : ModuleName -> Bool -> Definition -> Html
defHeaderView moduleName isEditing {name, tipe} =
  let
    nameTag = editable isEditing name "td" [ A.class "tag definition-name" ] [ H.text name ] 
    evalTag = H.td [ A.class "tag definition-evaluate", E.onClick (action (Evaluate (moduleName, name))) ] [ H.text "eval" ]
    header = case tipe of
      Just t ->
        [ nameTag, H.td [ A.class "tag definition-type" ] [ H.text t ], evalTag ]
      Nothing ->
        [ nameTag, evalTag ]
  in
    H.table [ A.class "definition-header" ]
      [ H.tr [] header ]

defView : Values -> ModuleName -> Maybe Name -> Definition -> Html
defView values moduleName editing definition =
  let
    {name, tipe, binding} = definition
    isEditing = Just name == editing
    class = if isEditing then "definition editing" else "definition"
    valueView = case (Dict.get name values) of
      Just value -> [ div "definition-value" [ H.text value ] ]
      Nothing -> []
  in
    H.div [ A.class class ] <|
      [ defHeaderView moduleName isEditing definition
      , codeView isEditing definition
      ] ++ valueView

compileButton : Module -> Html
compileButton program =
  button "compile-button" "compile" (Compile program)

moduleView : Values -> Maybe Name -> Module -> Html
moduleView values editing program =
  let
    {name, imports, adts, defs} = program
  in
    div "module"
      [ div "module-header"
        [ H.span [ A.class "module-name" ] [ H.text <| Module.nameToString name ], compileButton program ]
      , div "module-imports" <| List.map importView imports
      , div "module-adts" <| List.map adtView adts
      , div "module-defs" <| List.map (defView values name editing) defs ++ [button "new-def-button" "+" NewDefinition]
      ]
  
errorView : CompilationStatus -> Html
errorView status =
  case status of
    Ok _ -> div "no-error" []
    Err err -> div "error" [ H.pre [] [ H.text err ] ]

view : State -> Html
view {program, values, editing, compilationStatus} =
  div "modules" [ moduleView values editing program, errorView compilationStatus ]

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
    snd <~ S.keepIf fst (False, default) (decorate <~ maybes)

button : String -> String -> Action -> Html
button className buttonText act =
  H.div
    [ A.class className
    , E.onClick (action act)
    ]
    [ H.text buttonText ]

editable : Bool -> Name -> String -> List H.Attribute -> List Html -> Html
editable isEditing name tagName additionalAttrs contents =
  let
    attrs = [ A.contenteditable isEditing, editEvent isEditing name ] ++ additionalAttrs 
  in
    H.node tagName attrs contents

visible : Bool -> H.Attribute
visible v =
  A.style [("visibility", if v then "visible" else "hidden")]

div : String -> List Html -> Html
div class =
  H.div [A.class class]

--
-- Main
--

main : Signal Html
main =
  view <~ state
