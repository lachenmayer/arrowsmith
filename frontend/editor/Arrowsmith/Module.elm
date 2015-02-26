module Arrowsmith.Module where

import Array
import List (intersperse, isEmpty, map, filter)
import String (concat, join)

import Arrowsmith.Types (..)
import Arrowsmith.Definition as Def

newDefinition : Module -> Definition -> Module
newDefinition program def =
  { program | defs <- program.defs ++ [def] }

freshName : Int -> String
freshName fresh =
  let
    names = Array.fromList ["foo", "bar", "baz", "buz", "qux", "quux", "corge", "grault", "garply", "waldo", "fred", "plugh", "xyzzy", "thud"]
    nameCount = Array.length names
    name idx = case Array.get idx names of
      Just n -> n
      Nothing -> name ((idx // nameCount) - 1) ++ name (idx % nameCount)
  in
    name fresh


freshDefinition : Module -> Int -> Module
freshDefinition program fresh =
  newDefinition program { name = freshName fresh, tipe = Nothing, binding = undefinedBinding }

removeDefinition : Module -> Name -> Module
removeDefinition program toRemove =
  { program | defs <- filter (\{name} -> name /= toRemove) program.defs }

replaceDefinition : Module -> Name -> Definition -> Module
replaceDefinition program name newDefinition =
  let
    update oldDefinition =
      if oldDefinition.name == name then
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

helloWorld : Module
helloWorld =
  { name = ["Foo", "HelloWorld"]
  , imports =
    [ "Graphics.Element (..)"
    , "Text (..)"
    ]
  , adts = []
  , defs =
    [ {name = "numbers", tipe = Just "number", binding = "1 + 2"}
    , {name = "doubles", tipe = Just "number", binding = "numbers * 2"}
    ]
  }

undefinedBinding : String
undefinedBinding =
  "\"__undefined__\""