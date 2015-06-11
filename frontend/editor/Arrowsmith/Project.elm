module Arrowsmith.Project where

import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Json
import Signal as S exposing (Signal, Mailbox, Address, (<~))

import Arrowsmith.Types exposing (..)
import Arrowsmith.Util exposing (..)

port repo : Repo
port files : List String
port expanded : Bool

type Action
  = NoOp
  | Collapse
  | Expand

type alias Model =
  { repo : Repo
  , files : List String
  , expanded : Bool
  }

init : Model
init =
  { repo = repo
  , files = files
  , expanded = True
  }

actions : Mailbox Action
actions =
  S.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Collapse -> { model | expanded <- False }
    Expand -> { model | expanded <- True }

model : Signal Model
model =
  S.foldp update init actions.signal

view : Address Action -> Model -> Html
view address {repo, files, expanded} =
  let
    maybeFileList = if expanded then [ fileList repo files ] else []
  in
    div "project" <| [ repoHeader repo ] ++ maybeFileList

repoHeader : Repo -> Html
repoHeader {backend, user, project} =
  div "project-header"
    [ span "project-header-name" (user ++ "/" ++ project)
    ]

fileList : Repo -> List String -> Html
fileList {user, project} files =
  let
    fileUrl file = "/" ++ user ++ "/" ++ project ++ "/" ++ file
  in
    div "project-modules" <| List.map (\f -> H.a [ A.href (fileUrl f), A.class "project-module-link" ] [ div "project-module"  [ H.text f ] ]) files

main : Signal Html
main =
  view actions.address <~ model
