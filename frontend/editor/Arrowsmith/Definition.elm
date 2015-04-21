module Arrowsmith.Definition where

import String exposing (join)

import Arrowsmith.Types exposing (..)

toString : Definition -> String
toString (name, tipe, binding) =
  let
    lhs = name ++ " =" -- TODO arguments?
    rhs = "  " ++ binding -- TODO proper indentation
    def = case tipe of
      Just t ->
        [name ++ " : " ++ t, lhs, rhs]
      Nothing ->
        [lhs, rhs]
  in
    join "\n" def
