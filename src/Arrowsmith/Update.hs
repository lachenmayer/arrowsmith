module Arrowsmith.Update where

import Data.FileStore (TimeRange(..), Revision(..))

import Arrowsmith.Repo
import Arrowsmith.Types

-- gets all update annotations *after* the specified revision id
getUpdateAnnotations :: Repo -> ElmFile -> RevisionId -> IO [EditUpdate]
getUpdateAnnotations repo elmFile' revision = do
  fileRevisions <- history repo [(filePath elmFile')] (TimeRange Nothing Nothing) Nothing
  let followingRevisions = drop 1 . dropWhile (\rev -> revId rev /= revision) $ fileRevisions
  return $ map fst . concatMap reads . map revDescription $ followingRevisions
