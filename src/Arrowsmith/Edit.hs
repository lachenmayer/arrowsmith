module Arrowsmith.Edit where

import Data.List (delete)

import Arrowsmith.Definition
import Arrowsmith.Module
import Arrowsmith.Types
import Arrowsmith.Util


addDefinition :: Definition -> (ElmCode, Maybe Module) -> Maybe (ElmCode, Module)
addDefinition def@(defName, defType, defBinding) (source, maybeModule) = do
  modul' <- maybeModule
  let (_, _, _, lastStart, lastEnd) = last (defs modul')
      (before, lastDef, after) = breakSource source lastStart lastEnd
  return ( before ++ lastDef ++ "\n\n" ++ prettyPrint def ++ "\n" ++ after
         , modul' { defs = defs modul' ++ [(defName, defType, defBinding, (0, 0), (0,0))] }
         )

changeDefinition :: VarName -> ElmCode -> (ElmCode, Maybe Module) -> Maybe (ElmCode, Module)
changeDefinition varName elmCode (source, maybeModule) = do
  modul' <- maybeModule
  let defs' = defs modul'
  def <- definitionWithName defs' varName
  let (_, _, _, defStart, defEnd) = def
      (before, _, after) = breakSource source defStart defEnd
      newDef = (varName, Nothing, elmCode, defStart, defEnd) -- TODO wrong def{Start,End}
  return ( before ++ prettyPrintLocated newDef ++ after
         , modul' { defs = update def newDef defs' }
         )

removeDefinition :: VarName -> (ElmCode, Maybe Module) -> Maybe (ElmCode, Module)
removeDefinition varName (source, maybeModule) = do
  modul' <- maybeModule
  let defs' = defs modul'
  def <- definitionWithName defs' varName
  let (_, _, _, defStart, defEnd) = def
      (before, _, after) = breakSource source defStart defEnd
  return ( before ++ after
         , modul' { defs = delete def defs' }
         )

performAction :: Action -> (ElmCode, Maybe Module) -> Maybe (ElmCode, Module)
performAction action' =
  case action' of
    AddDefinition def -> addDefinition def
    ChangeDefinition varName elmCode -> changeDefinition varName elmCode
    RemoveDefinition varName -> removeDefinition varName
