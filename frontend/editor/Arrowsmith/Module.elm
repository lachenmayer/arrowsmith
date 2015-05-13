module Arrowsmith.Module where

import Array
import Dict
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

--toString : Module -> ElmCode
--toString {name, imports, types, defs} =
--  let
--    lines = join "\n"
--    moduleDeclaration = "module " ++ nameToString name ++ " where"
--    definitions = map Def.toString defs
--  in
--    join "\n\n"
--      [ moduleDeclaration
--      --, lines (map ((++) "import ") imports) -- TODO
--      , lines types
--      , lines definitions
--      ]

empty : Module
empty =
  { name = ["Program"]
  , imports = []
  , types = Dict.empty
  , defs = []
  , errors = []
  }

fromPort : PortModule -> Module
fromPort portModule =
  { name = portModule.name
  , imports = portModule.imports
  , types = Dict.singleton "encode" (Var "foo")
  , defs = portModule.defs
  , errors = portModule.errors
  }