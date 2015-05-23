module Arrowsmith.ModuleView (Model, Action(..), init, update, view) where

import Debug

import Color
import Dict as D exposing (Dict)
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

import Arrowsmith.Definition as Def
import Arrowsmith.ImportsView as ImportsView
import Arrowsmith.Module as Module
import Arrowsmith.Types exposing (..)

type alias Model =
  { modul : Module

  , isCompiling : Bool
  , compileStatus : CompileStatus

  , editing : Maybe VarName

  , valueViews : ValueViews
  , toEvaluate : List (VarName, ModuleName)

  , lastAction : Action

  , importsViewModel : ImportsView.Model
  }

type Action
  = NoOp

  | Edit VarName
  | StopEditing VarName
  | FinishEditing (VarName, String)

  | Evaluate VarName ModuleName {- view module name -}
  | EvaluateMain
  | FinishEvaluating (ModuleName, VarName, ModuleName)

  | NewDefinition
  | RemoveDefinition VarName

  | ModuleCompiled Module
  | CompilationFailed ElmError

  | ChangeImports ImportsView.Action

init : Module -> Model
init initialModule =
  { modul = initialModule

  , isCompiling = False
  , compileStatus = Compiled

  , editing = Nothing

  , valueViews = D.empty
  , toEvaluate = []

  , lastAction = NoOp

  , importsViewModel = ImportsView.init initialModule.imports
  }

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Edit name ->
      { model
      | lastAction <- Edit name
      , editing <- Just name
      }
    StopEditing name ->
      { model -- Nothing happens, this is only used to call the JS "edit" function.
      | lastAction <- StopEditing name
      }
    FinishEditing (name, newBinding) ->
      { model
      | lastAction <- FinishEditing (name, newBinding)
      , editing <- Nothing
      , modul <- Module.replaceDefinition model.modul name (name, Nothing, newBinding)
      , toEvaluate <- D.toList model.valueViews
      }

    Evaluate e view ->
      { model
      | lastAction <- Evaluate e view
      , toEvaluate <- [(e, view)]
      }
    EvaluateMain ->
      { model
      | lastAction <- EvaluateMain
      }
    FinishEvaluating (moduleName, name, view) ->
      { model
      | lastAction <- FinishEvaluating (moduleName, name, view)
      , valueViews <- D.insert name view model.valueViews
      }

    ModuleCompiled newModule ->
      { model
      | lastAction <- ModuleCompiled newModule
      , compileStatus <- Compiled
      , modul <- newModule
      }
    CompilationFailed error ->
      { model
      | lastAction <- CompilationFailed error
      , compileStatus <- CompileError error
      }

    ChangeImports action ->
      { model
      | lastAction <- ChangeImports action
      , importsViewModel <- ImportsView.update action model.importsViewModel
      }

view : S.Address Action -> Model -> Html
view address {modul, valueViews, compileStatus, importsViewModel} =
  div "modules" [ moduleView address valueViews modul importsViewModel ]

--
-- Views
--

moduleView : S.Address Action -> ValueViews -> Module -> ImportsView.Model -> Html
moduleView address valueViews modul importsViewModel =
  let
    {name, types, defs, errors} = modul
    moduleDefsClass = if errors == [] then "module-defs" else "module-defs module-has-error"
  in
    div ("module module-" ++ (String.join "-" name))
      [ div "module-header"
        [ H.span [ A.class "module-name" ] [ H.text <| Module.nameToString name ]
        , H.span [ A.class "module-evaluate", E.onClick address EvaluateMain ] [ FontAwesome.play Color.white 16 ]
        ]
      , ImportsView.view (S.forwardTo address ChangeImports) importsViewModel
      --, div "module-adts" <| List.map datatypeView datatypes
      , div moduleDefsClass <| List.map (defView address types valueViews) defs ++ [newDefView address]
      , div "module-errors" <| List.map errorView errors
      ]

typeView : ElmCode -> Html
typeView code =
  div "datatype" [ H.code [] [ H.text code ] ]

defView : S.Address Action -> List (VarName, Type) -> ValueViews -> Definition -> Html
defView address inferredTypes valueViews definition =
  let
    (name, tipe, binding) = definition
    class = "definition defname-" ++ name
  in
    H.div [ A.class class ] <|
      [ defHeaderView address inferredTypes valueViews definition
      , codeView address definition
      ]

defHeaderView : S.Address Action -> List (VarName, Type) -> ValueViews -> Definition -> Html
defHeaderView address inferredTypes valueViews (name, tipe, _) =
  let
    nameTag = tag "definition-name" [] [ H.text name ]
    evalTag = tag "definition-evaluate" [ E.onClick address (Evaluate name (valueView actualType)) ] [ FontAwesome.play Color.white 16 ]
    actualType = case tipe of
      Just t -> t
      Nothing -> lookup name inferredTypes
    header = case tipe of
      Just t ->
        [ nameTag, tag "definition-type" [] [ H.text t ], evalTag ]
      Nothing ->
        [ nameTag, tag "definition-type-inferred" [] [ H.text <| lookup name inferredTypes ], evalTag ]
  in
    H.table [ A.class "definition-header" ]
      [ H.tr [] header ]

valueView : Type -> ModuleName
valueView tipe =
  case (Debug.log "type" tipe) of
    "Color.Color" -> ["Arrowsmith", "Views", "ColorView"]
    _ -> ["Arrowsmith", "Views", "SimpleView"]

codeView : S.Address Action -> Definition -> Html
codeView address (name, tipe, binding) =
  let
    lineCount = List.length (String.lines binding)
  in
    editable address name "textarea"
      [ A.class "definition-code", A.rows lineCount ]
      [ H.text binding ]

newDefView : S.Address Action -> Html
newDefView address =
  H.div [ A.class "definition new-definition" ] <|
    [ codeView address Def.newDefinition ]

errorView : ElmError -> Html
errorView error =
  div "error" [ H.pre [] [ H.text error ] ]

button : S.Address Action -> String -> String -> Action -> Html
button address className buttonText act =
  H.div
    [ A.class className
    , E.onClick address act
    ]
    [ H.text buttonText ]

editable : S.Address Action -> VarName -> String -> List H.Attribute -> List Html -> Html
editable address name tagName additionalAttrs contents =
  let
    attrs = A.contenteditable True :: editActions address name ++ additionalAttrs
  in
    H.node tagName attrs contents

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

editActions : S.Address Action -> VarName -> List H.Attribute
editActions address name =
  [ E.onFocus address (Edit name), E.onBlur address (StopEditing name) ]

visible : Bool -> H.Attribute
visible v =
  A.style [ ("visibility", if v then "visible" else "hidden") ]

div : String -> List Html -> Html
div class =
  H.div [ A.class class ]

tag : String -> List H.Attribute -> List Html -> Html
tag class attrs =
  H.td (A.class ("tag " ++ class) :: attrs)

--lookup : a -> List (a, b) -> b
lookup x xs =
  snd << Maybe.withDefault ("","") << List.head <| List.filter (fst >> (==) x) xs