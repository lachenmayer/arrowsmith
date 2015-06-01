module Arrowsmith.StructuredEditView (Action(..), Model, init, update, view) where

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

  | Evaluate VarName ModuleName {- view module name -}
  | FinishEvaluating (ModuleName, VarName, ModuleName)

  | EvaluateMain
  | StopPlaying

  | NewDefinition
  | RemoveDefinition VarName

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
  , infoViewsExpanded : Bool
  , aliasesViewModel : AliasesView.Model
  , datatypesViewModel : DatatypesView.Model
  , importsViewModel : ImportsView.Model
  }

init : Module -> Model
init initialModule =
  { modul = initialModule
  , editing = Nothing
  , playing = False
  , valueViews = D.empty
  , toEvaluate = []
  , infoViewsExpanded = False
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
      { model
      | editing <- Nothing
      , toEvaluate <- D.toList model.valueViews
      }

    Evaluate e view ->
      { model
      | toEvaluate <- D.toList <| D.insert e view model.valueViews
      }
    FinishEvaluating (moduleName, name, view) ->
      { model
      | valueViews <- D.insert name view model.valueViews
      }

    EvaluateMain ->
      { model
      | playing <- True
      }
    StopPlaying ->
      { model
      | playing <- False
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
    ] ++ if model.playing then [] else [ definitionsView address model.modul model.valueViews ]

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

definitionsView : Address Action -> Module -> ValueViews -> Html
definitionsView address modul valueViews =
  let
    {name, types, defs} = modul
  in
    div "module-defs" <| List.map (defView address types valueViews) defs ++ [newDefView address]

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
    div "definition-header" header

valueView : Type -> ModuleName
valueView tipe =
  case tipe of
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
