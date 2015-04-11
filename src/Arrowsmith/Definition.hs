module Arrowsmith.Definition where

import Data.List (find)

import Arrowsmith.Types


prettyPrintLocated :: LocatedDefinition -> String
prettyPrintLocated (name', maybeType, binding, _, _) =
  prettyPrint (name', maybeType, binding)

prettyPrint :: Definition -> String
prettyPrint (name', maybeType, binding) =
  case maybeType of
    Nothing -> binding -- contains lhs
    Just tipe -> name' ++ " : " ++ tipe ++ "\n" ++ binding

definitionWithName :: [LocatedDefinition] -> VarName -> Maybe LocatedDefinition
definitionWithName defs' defName =
  find (\(name', _, _, _, _) -> name' == defName) defs'