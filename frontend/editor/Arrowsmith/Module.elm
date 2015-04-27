module Arrowsmith.Module where

import Array
import List exposing (intersperse, isEmpty, map, filter)
import String exposing (concat, join)

import Arrowsmith.Types exposing (..)
import Arrowsmith.Definition as Def

newDefinition : Module -> Definition -> Module
newDefinition program def =
  { program | defs <- program.defs ++ [def] }

removeDefinition : Module -> Name -> Module
removeDefinition program toRemove =
  { program | defs <- filter (\(name, _, _) -> name /= toRemove) program.defs }

replaceDefinition : Module -> Name -> Definition -> Module
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

nameToString : ModuleName -> String
nameToString name =
  if isEmpty name then "Program" else concat <| intersperse "." name

toString : Module -> ElmCode
toString {name, imports, adts, defs} =
  let
    lines = join "\n"
    moduleDeclaration = "module " ++ nameToString name ++ " where"
    definitions = map Def.toString defs
  in
    join "\n\n"
      [ moduleDeclaration
      , lines (map ((++) "import ") imports)
      , lines adts
      , lines definitions
      ]

empty : Module
empty =
  { name = ["Program"]
  , imports = []
  , adts = []
  , defs = []
  }
