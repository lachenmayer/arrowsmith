module Update where

import Arrowsmith.Types

update action =
  case action of
    AddDefinition moduleName def ->
    ChangeDefinition moduleName defName partialDef -> undefined
    RemoveDefinition moduleName defName -> undefined