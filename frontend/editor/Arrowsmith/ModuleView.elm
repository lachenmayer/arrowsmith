module Arrowsmith.ModuleView (Model, Action(..), init, update, view) where

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
import Arrowsmith.Module as Module
import Arrowsmith.Types exposing (..)

type alias Model =
  { modul : Module

  , isCompiling : Bool
  , compileStatus : CompileStatus
  , synced : Bool -- The code has changed (eg. by editing), but it has not been recompiled yet.

  , editing : Maybe Name

  , values : Dict Name Value
  , toEvaluate : Maybe (ModuleName, Name)

  , fresh : Int
  }

type Action
  = NoOp

  | Edit Name
  | StopEditing Name
  | FinishEditing (Name, Value)

  | Evaluate (ModuleName, Name)
  | FinishEvaluating (ModuleName, Name, Value)

  | NewDefinition
  | RemoveDefinition Name

  | ModuleCompiled Module
  | CompilationFailed ElmError

init : Module -> Model
init initialModule =
  { modul = initialModule

  , isCompiling = False
  , compileStatus = Compiled
  , synced = True

  , editing = Nothing

  , values = D.empty
  , toEvaluate = Nothing

  , fresh = 0
  }

update : Action -> Model -> Model
update action model =
  --Debug.log "model" <| case action of
  case (Debug.log "ModuleView:action" action) of
    NoOp ->
      model

    Edit name ->
      { model | editing <- Just name }
    StopEditing _ ->
      model -- Nothing happens, this is only used to call the JS "edit" function.
    FinishEditing (name, newBinding) ->
      { model
      | editing <- Nothing
      , modul <- Module.replaceDefinition model.modul name (name, Nothing, newBinding)
      }

    Evaluate e ->
      { model | toEvaluate <- Just e }
    FinishEvaluating (moduleName, name, value) ->
      { model
      | values <- D.insert name value model.values
      , toEvaluate <- Nothing
      }

    ModuleCompiled newModule ->
      { model
      | compileStatus <- Compiled
      , synced <- True
      , modul <- newModule
      }
    CompilationFailed error ->
      { model
      | compileStatus <- CompileError error
      , synced <- True
      }

view : S.Address Action -> Model -> Html
view address {modul, compileStatus} =
  div "modules" [ moduleView address modul ]

--
-- Views
--

moduleView : S.Address Action -> Module -> Html
moduleView address modul =
  let
    {name, imports, datatypes, defs, errors} = modul
  in
    div "module"
      [ div "module-header"
        [ H.span [ A.class "module-name" ] [ H.text <| Module.nameToString name ] ]
      , div "module-imports" <| List.map importView imports
      , div "module-adts" <| List.map datatypeView datatypes
      , div "module-defs" <| List.map (defView address name) defs ++ [newDefView address]
      , div "module-errors" <| List.map errorView errors
      ]

importView : ElmCode -> Html
importView code =
  div "import" [ H.text code ]

datatypeView : ElmCode -> Html
datatypeView code =
  div "datatype" [ H.code [] [ H.text code ] ]

defView : S.Address Action -> ModuleName -> Definition -> Html
defView address moduleName definition =
  let
    (name, tipe, binding) = definition
    class = "definition defname-" ++ name
  in
    H.div [ A.class class ] <|
      [ defHeaderView address moduleName definition
      , codeView address definition
      ]

defHeaderView : S.Address Action -> ModuleName -> Definition -> Html
defHeaderView address moduleName (name, tipe, _) =
  let
    nameTag = tag "definition-name" [] [ H.text name ]
    evalTag = tag "definition-evaluate" [ E.onClick address (Evaluate (moduleName, name)) ] [ H.text "eval" ]
    header = case tipe of
      Just t ->
        [ nameTag, tag "definition-type" [] [ H.text t ], evalTag ]
      Nothing ->
        [ nameTag, evalTag ]
  in
    H.table [ A.class "definition-header" ]
      [ H.tr [] header ]

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

editable : S.Address Action -> Name -> String -> List H.Attribute -> List Html -> Html
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

editActions : S.Address Action -> Name -> List H.Attribute
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
