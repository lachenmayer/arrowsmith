module Arrowsmith.Update where

import Arrowsmith.Definition
import Arrowsmith.ElmFile
import Arrowsmith.Module
import Arrowsmith.Types

performUpdate :: ElmFile -> Action -> IO (Either String ElmFile)
performUpdate elmFile' action =
  case action of
    AddDefinition moduleName def -> do
      case modul elmFile' of
        Nothing -> return $ Left "Update failed - could not compile Elm source."
        Just modul' ->
          changeSource elmFile' (show action) $ \source ->
            let
              (_, _, _, lastStart, lastEnd) = last (defs modul')
              (before, lastDef, after) = breakSource source lastStart lastEnd
            in
              before ++ lastDef ++ "\n\n" ++ prettyPrint def  ++ "\n" ++ after
    ChangeDefinition moduleName defName partialDef -> error "not implemented"
    RemoveDefinition moduleName defName -> error "not implemented"

