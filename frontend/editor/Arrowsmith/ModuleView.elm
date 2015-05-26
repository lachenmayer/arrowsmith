module Arrowsmith.ModuleView (Action(..), Model, init, update, view) where

import Debug

import Color
import Dict as D exposing (Dict)
import FontAwesome
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Signal as S exposing (Signal, Mailbox, Address, (<~))
import String

import Arrowsmith.AliasesView as AliasesView
import Arrowsmith.Definition as Def
import Arrowsmith.DatatypesView as DatatypesView
import Arrowsmith.ImportsView as ImportsView
import Arrowsmith.Module as Module
import Arrowsmith.Types exposing (..)
import Arrowsmith.Util exposing (..)


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

  | ChangeAliases AliasesView.Action
  | ChangeDatatypes DatatypesView.Action
  | ChangeImports ImportsView.Action

type alias Model =
  { modul : Module
  , editing : Maybe VarName
  , valueViews : ValueViews
  , toEvaluate : List (VarName, ModuleName)
  , aliasesViewModel : AliasesView.Model
  , datatypesViewModel : DatatypesView.Model
  , importsViewModel : ImportsView.Model
  }

init : Module -> Model
init initialModule =
  { modul = initialModule
  , editing = Nothing
  , valueViews = D.empty
  , toEvaluate = []
  , aliasesViewModel = AliasesView.init initialModule.aliases
  , datatypesViewModel = DatatypesView.init initialModule.datatypes
  , importsViewModel = ImportsView.init initialModule.imports
  }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Edit name ->
      { model
      | editing <- Just name
      }
    StopEditing name ->
      model -- Nothing happens, this is only used to call the JS "edit" function.
    FinishEditing (name, newBinding) ->
      { model
      | editing <- Nothing
      , modul <- Module.replaceDefinition model.modul name (name, Nothing, newBinding)
      , toEvaluate <- D.toList model.valueViews
      }

    Evaluate e view ->
      { model
      | toEvaluate <- [(e, view)]
      }
    EvaluateMain ->
      model
    FinishEvaluating (moduleName, name, view) ->
      { model
      | valueViews <- D.insert name view model.valueViews
      }

    ModuleCompiled newModule ->
      { model
      | modul <- newModule
      }
    CompilationFailed error ->
      model

    ChangeAliases action ->
      { model
      | aliasesViewModel <- AliasesView.update action model.aliasesViewModel
      }
    ChangeDatatypes action ->
      { model
      | datatypesViewModel <- DatatypesView.update action model.datatypesViewModel
      }
    ChangeImports action ->
      { model
      | importsViewModel <- ImportsView.update action model.importsViewModel
      }

model : Module -> Signal Model
model initialModule =
  S.foldp update (init initialModule) actions.signal

view : Address Action -> Model -> Html
view address {valueViews, modul, importsViewModel, datatypesViewModel, aliasesViewModel} =
  let
    {name, types, defs, errors} = modul
    moduleDefsClass = if errors == [] then "module-defs" else "module-defs module-has-error"
  in
    H.div [ A.class "module-editor structured-editor" ]
      [ ImportsView.view (S.forwardTo address ChangeImports) importsViewModel
      , AliasesView.view (S.forwardTo address ChangeAliases) aliasesViewModel
      , DatatypesView.view (S.forwardTo address ChangeDatatypes) datatypesViewModel
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
    editable "textarea"
      [ A.class "definition-code"
      , A.rows lineCount
      , E.onFocus address (Edit name)
      , E.onBlur address (StopEditing name)
      ]
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
