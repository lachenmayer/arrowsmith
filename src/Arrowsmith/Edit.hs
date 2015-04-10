module Arrowsmith.Edit where

import Arrowsmith.Definition
import Arrowsmith.ElmFile
import Arrowsmith.Module
import Arrowsmith.Types

addDefinition :: Definition -> (ElmCode, Maybe Module) -> (ElmCode, Maybe Module)
addDefinition def@(defName, defType, defBinding) (source, maybeModule) =
  case maybeModule of
    Nothing ->
      (source ++ prettyPrint def, Nothing)
    Just modul' ->
      ( before ++ lastDef ++ "\n\n" ++ prettyPrint def ++ "\n" ++ after
      -- TODO position
      , Just modul' { defs = defs modul' ++ [(defName, defType, defBinding, (0, 0), (0,0))] }
      )
      where
        (_, _, _, lastStart, lastEnd) = last (defs modul')
        (before, lastDef, after) = breakSource source lastStart lastEnd

performEdit :: ElmFile -> Action -> IO ElmFile
performEdit elmFile' action' =
  edit elmFile' $ case action' of
    AddDefinition def -> addDefinition def
    RemoveDefinition varName -> error "not implemented"
