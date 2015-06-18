module Arrowsmith.StructuredEditView (Action(..), Model, init, update, view) where

import Debug

import Color
import Dict as D exposing (Dict)
import FontAwesome
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Set exposing (Set)
import Signal as S exposing (Signal, Mailbox, Address, (<~))
import String

import Arrowsmith.AliasesView as AliasesView
import Arrowsmith.Definition as Def
import Arrowsmith.DatatypesView as DatatypesView
import Arrowsmith.ImportsView as ImportsView
import Arrowsmith.Module as Module
import Arrowsmith.Types exposing (..)
import Arrowsmith.Util exposing (..)

-- Value views
import Arrowsmith.Views.ColorView
import Arrowsmith.Views.DictView
import Arrowsmith.Views.FormView
import Arrowsmith.Views.GraphView
import Arrowsmith.Views.ListView
import Arrowsmith.Views.SimpleView


type Action
  = NoOp

  | Edit VarName
  | StopEditing VarName

  | Evaluate VarName ModuleName {- view module name -}
  | FinishEvaluating (ModuleName, VarName, ModuleName)
  | StopEvaluating VarName

  | EvaluateMain
  | StopPlaying

  | CollapseDefinition VarName
  | ExpandDefinition VarName

  | ChangeModule Module
  | ChangeAliases AliasesView.Action
  | ChangeDatatypes DatatypesView.Action
  | ChangeImports ImportsView.Action

  | ExpandInfoViews
  | ChangeEditorView

type alias Model =
  { modul : Module
  , editing : Maybe VarName
  , playing : Bool
  , valueViews : ValueViews
  , toEvaluate : List (VarName, ModuleName)
  , collapsedDefinitions : Set VarName
  , infoViewsExpanded : Bool
  , aliasesViewModel : AliasesView.Model
  , datatypesViewModel : DatatypesView.Model
  , importsViewModel : ImportsView.Model
  }

type alias DefinitionViewModel =
  { def : Definition
  , inferredType : Type
  , valueView : ModuleName
  , hasValue : Bool
  , isCollapsed : Bool
  }

init : Module -> Model
init initialModule =
  { modul = initialModule
  , editing = Nothing
  , playing = False
  , valueViews = D.empty
  , toEvaluate = []
  , collapsedDefinitions = Set.empty
  , infoViewsExpanded = False
  , aliasesViewModel = AliasesView.init initialModule.aliases
  , datatypesViewModel = DatatypesView.init initialModule.datatypes
  , importsViewModel = ImportsView.init initialModule.imports
  }

valueViews : List ViewInfo
valueViews =
  [ Arrowsmith.Views.ColorView.info
  , Arrowsmith.Views.DictView.info
  , Arrowsmith.Views.FormView.info
  , Arrowsmith.Views.GraphView.info
  , Arrowsmith.Views.ListView.info
  ]

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
      { model
      | editing <- Nothing
      , toEvaluate <- D.toList model.valueViews
      }

    Evaluate name view ->
      let
        newValueViews = D.insert name view model.valueViews
      in
        { model
        | toEvaluate <- D.toList newValueViews
        , valueViews <- newValueViews
        }
    FinishEvaluating (moduleName, name, view) ->
      { model
      | toEvaluate <- []
      }
    StopEvaluating name ->
      { model
      | valueViews <- D.remove name model.valueViews
      }

    EvaluateMain ->
      { model
      | playing <- True
      }
    StopPlaying ->
      { model
      | playing <- False
      }

    CollapseDefinition varName ->
      { model
      | collapsedDefinitions <- Set.insert varName model.collapsedDefinitions
      }
    ExpandDefinition varName ->
      { model
      | collapsedDefinitions <- Set.remove varName model.collapsedDefinitions
      }

    ChangeModule modul ->
      { model
      | modul <- modul
      }
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

    ExpandInfoViews ->
      { model
      | infoViewsExpanded <- toggle model.infoViewsExpanded
      }
    ChangeEditorView ->
      model

model : Module -> Signal Model
model initialModule =
  S.foldp update (init initialModule) actions.signal

view : Address Action -> Model -> Html
view address model =
  div "module-editor structured-editor" <|
    [ actionsView address model.playing
    , infoViews address model
    ] ++ if model.playing then [] else [ definitionsView address model ]

actionsView : Address Action -> Bool -> Html
actionsView address playing =
  let
    button action icon =
      H.span [ A.class "action-button", E.onClick address action ] [ icon (Color.rgb 33 33 33) 24 ]
  in
    div "module-actions"
      [ div "action-buttons"
        [ button ExpandInfoViews FontAwesome.info
        , button ChangeEditorView FontAwesome.file_text
        , if playing then button StopPlaying FontAwesome.pause else button EvaluateMain FontAwesome.play
        ]
      ]

infoViews : Address Action -> Model -> Html
infoViews address model =
  div (if model.infoViewsExpanded then "module-info-views visible" else "module-info-views hidden")
    [ ImportsView.view (S.forwardTo address ChangeImports) model.importsViewModel
    , AliasesView.view (S.forwardTo address ChangeAliases) model.aliasesViewModel
    , DatatypesView.view (S.forwardTo address ChangeDatatypes) model.datatypesViewModel
    ]

typeView : ElmCode -> Html
typeView code =
  div "datatype" [ H.code [] [ H.text code ] ]

definitionsView : Address Action -> Model -> Html
definitionsView address {modul, valueViews, collapsedDefinitions} =
  let
    {name, types, defs} = modul
    defViewModel (name, tipe, binding) =
      let
        inferredType = lookup name types
      in
        { def = (name, tipe, binding)
        , inferredType = inferredType
        , valueView = valueView inferredType
        , hasValue = D.member name valueViews
        , isCollapsed = D.member name collapsedDefinitions
        }
  in
    div "module-defs" <|
      List.map (defView address << defViewModel) defs ++ [newDefView address]

defView : S.Address Action -> DefinitionViewModel -> Html
defView address definition =
  let
    (name, _, _) = definition.def
    class = "definition defname-" ++ name ++ if definition.isCollapsed then " collapsed" else " expanded"
    code = if definition.isCollapsed then [] else [ codeView address definition.def ]
  in
    H.div [ A.class class ] <|
      [ defHeaderView address definition ] ++ code

defHeaderView : S.Address Action -> DefinitionViewModel -> Html
defHeaderView address {def, inferredType, valueView, hasValue, isCollapsed} =
  let
    (name, tipe, _) = def
    collapseAction = if isCollapsed then ExpandDefinition name else CollapseDefinition name
    nameTag = tag "definition-name" [ E.onClick address collapseAction ] [ H.text name ]
    evalTag = if hasValue then
      tag "definition-evaluate" [ E.onClick address (StopEvaluating name) ] [ FontAwesome.pause Color.white 16 ]
    else
      tag "definition-evaluate" [ E.onClick address (Evaluate name valueView) ] [ FontAwesome.play Color.white 16 ]
    header = case tipe of
      Just t ->
        [ nameTag, tag "definition-type" [] [ H.text t ], evalTag ]
      Nothing ->
        [ nameTag, tag "definition-type-inferred" [] [ H.text inferredType ], evalTag ]
  in
    div "definition-header" header

valueView : Type -> ModuleName
valueView tipe =
  valueViews
    |> List.filter (\i -> i.matches tipe)
    |> List.head
    |> Maybe.withDefault Arrowsmith.Views.SimpleView.info
    |> .name

codeView : S.Address Action -> Definition -> Html
codeView address (name, tipe, binding) =
  let
    lineCount = List.length (String.lines binding)
  in
    editable "textarea"
      [ A.class "definition-code"
      , A.rows lineCount
      , A.spellcheck False
      , E.onFocus address (Edit name)
      , E.onBlur address (StopEditing name)
      ]
      [ H.text binding ]

newDefView : S.Address Action -> Html
newDefView address =
  H.div [ A.class "definition new-definition" ] <|
    [ codeView address Def.newDefinition ]
