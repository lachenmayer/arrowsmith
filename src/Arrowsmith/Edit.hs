module Arrowsmith.Edit where

import Data.List (delete)

import Arrowsmith.Definition
import Arrowsmith.Module
import Arrowsmith.Types
import Arrowsmith.Util


addDefinition :: Definition -> ElmFile -> Maybe ElmFile
addDefinition def@(defName, defType, defBinding) elmFile' = do
  modul' <- modul elmFile'
  let (_, _, _, lastStart, lastEnd) = last (defs modul')
      (before, lastDef, after) = breakSource (source elmFile') lastStart lastEnd
  return elmFile'
    { source = before ++ lastDef ++ "\n\n" ++ prettyPrint def ++ "\n" ++ after
    , modul = Just modul' { defs = defs modul' ++ [(defName, defType, defBinding, (0, 0), (0,0))] }
    }

changeDefinition :: VarName -> ElmCode -> ElmFile -> Maybe ElmFile
changeDefinition varName elmCode elmFile' = do
  modul' <- modul elmFile'
  let defs' = defs modul'
  def <- definitionWithName defs' varName
  let (_, _, _, defStart, defEnd) = def
      (before, _, after) = breakSource (source elmFile') defStart defEnd
      newDef = (varName, Nothing, elmCode, defStart, defEnd) -- TODO wrong def{Start,End}
  return elmFile'
    { source = before ++ prettyPrintLocated newDef ++ after
    , modul = Just modul' { defs = update def newDef defs' }
    }

removeDefinition :: VarName -> ElmFile -> Maybe ElmFile
removeDefinition varName elmFile' = do
  modul' <- modul elmFile'
  let defs' = defs modul'
  def <- definitionWithName defs' varName
  let (_, _, _, defStart, defEnd) = def
      (before, _, after) = breakSource (source elmFile') defStart defEnd
  return elmFile'
    { source = before ++ after
    , modul = Just modul' { defs = delete def defs' }
    }

performAction :: Action -> ElmFile -> Maybe ElmFile
performAction action' =
  case action' of
    AddDefinition def -> addDefinition def
    ChangeDefinition varName elmCode -> changeDefinition varName elmCode
    RemoveDefinition varName -> removeDefinition varName
