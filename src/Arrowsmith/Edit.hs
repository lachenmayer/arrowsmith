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
      (lastEndRow, _) = lastEnd
      newStart = (lastEndRow + 2, 0)
      defSource = prettyPrint def
      newDef = (defName, defType, defBinding, newStart, endLocation newStart defSource)
  return elmFile'
    { source = before ++ lastDef ++ "\n\n" ++ defSource ++ "\n" ++ after
    , modul = Just modul' { defs = defs modul' ++ [newDef] }
    }

changeDefinition :: VarName -> ElmCode -> ElmFile -> Maybe ElmFile
changeDefinition varName' elmCode elmFile' = do
  modul' <- modul elmFile'
  let defs' = defs modul'
  def <- definitionWithName defs' varName'
  let (_, _, _, defStart, defEnd) = def
      (before, _, after) = breakSource (source elmFile') defStart defEnd
      defSource = prettyPrintLocated newDef
      newDef = (varName', Nothing, elmCode, defStart, endLocation defStart defSource)
  return elmFile'
    { source = before ++ defSource ++ after
    , modul = Just modul' { defs = update def newDef defs' }
    }

removeDefinition :: VarName -> ElmFile -> Maybe ElmFile
removeDefinition varName' elmFile' = do
  modul' <- modul elmFile'
  let defs' = defs modul'
  def <- definitionWithName defs' varName'
  let (_, _, _, defStart, defEnd) = def
      (before, _, after) = breakSource (source elmFile') defStart defEnd
  return elmFile'
    { source = before ++ after
    , modul = Just modul' { defs = delete def defs' }
    }

replaceText :: String -> ElmFile -> Maybe ElmFile
replaceText newSource elmFile' = do
  return elmFile' { source = newSource, modul = Nothing, compiledCode = Nothing }

performEditAction :: EditAction -> ElmFile -> Maybe ElmFile
performEditAction action' =
  case action' of
    AddDefinition def -> addDefinition def
    ChangeDefinition varName' elmCode -> changeDefinition varName' elmCode
    RemoveDefinition varName' -> removeDefinition varName'
    ReplaceText newSource -> replaceText newSource
