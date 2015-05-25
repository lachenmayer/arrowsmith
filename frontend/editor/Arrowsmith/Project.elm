module Arrowsmith.Project where

import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Json
import Signal as S exposing (Signal, Mailbox, Address, (<~))

import Arrowsmith.Types exposing (..)

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
    H.div [] <| [ repoHeader repo ] ++ maybeFileList

repoHeader : Repo -> Html
repoHeader {backend, user, project} =
  H.div [] [ H.text (backend ++ "/" ++ user ++ "/" ++ project)]

fileList : Repo -> List String -> Html
fileList {user, project} files =
  let
    fileUrl file = "/" ++ user ++ "/" ++ project ++ "/" ++ file
  in
    H.div [] <| List.map (\f -> H.div [] [ H.a [ A.href (fileUrl f) ] [ H.text f ] ]) files

main : Signal Html
main =
  view actions.address <~ model
