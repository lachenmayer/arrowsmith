module Arrowsmith.Module where

import Array
import List exposing (intersperse, isEmpty, map, filter)
import String exposing (concat, join)

import Arrowsmith.Types exposing (..)
import Arrowsmith.Definition as Def

newDefinition : Module -> Definition -> Module
newDefinition program def =
  { program | defs <- program.defs ++ [def] }

removeDefinition : Module -> VarName -> Module
removeDefinition program toRemove =
  { program | defs <- filter (\(name, _, _) -> name /= toRemove) program.defs }

replaceDefinition : Module -> VarName -> Definition -> Module
replaceDefinition program name newDefinition =
  let
    update oldDefinition =
      let (oldName, _, _) = oldDefinition in
        if oldName == name then
          newDefinition
        else
          oldDefinition
  in
    { program | defs <- map update program.defs }

nameToString : Name -> String
nameToString name =
  if isEmpty name then "Program" else concat <| intersperse "." name

empty : Module
empty =
  { name = ["Program"]
  , imports = []
  , types = []
  , defs = []
  , errors = []
  }
