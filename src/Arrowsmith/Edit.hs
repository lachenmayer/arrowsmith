module Arrowsmith.Edit where

import Arrowsmith.Definition
import Arrowsmith.ElmFile
import Arrowsmith.Module
import Arrowsmith.Types

performEdit :: ElmFile -> Action -> IO (Either String ElmFile)
performEdit elmFile' action =
  case action of
    AddDefinition def ->
      edit elmFile' (show action) (addSourceChange def) (addModuleChange def)
    RemoveDefinition defName ->
      error "not implemented"
  where
    addSourceChange def source modul' =
      before ++ lastDef ++ "\n\n" ++ prettyPrint def ++ "\n" ++ after
      where
        (_, _, _, lastStart, lastEnd) = last (defs modul')
        (before, lastDef, after) = breakSource source lastStart lastEnd

    addModuleChange def source modul' =
      modul' { defs = defs modul' ++ [let (n, t, c) = def in (n, t, c, (0, 0), (0, 0))] }