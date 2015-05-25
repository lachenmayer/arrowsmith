module Arrowsmith.Update where

import Control.Monad (foldM)
import Data.FileStore (TimeRange(..), Revision(..))

import Arrowsmith.Edit
import Arrowsmith.Repo
import Arrowsmith.Types

-- gets all update annotations *after* the specified revision id
getUpdateAnnotations :: Repo -> ElmFile -> RevisionId -> IO [EditUpdate]
getUpdateAnnotations repo elmFile' revision = do
  fileRevisions <- history repo [(filePath elmFile')] (TimeRange Nothing Nothing) Nothing
  let followingRevisions = takeWhile (\rev -> revId rev /= revision) $ fileRevisions
  return $ map fst . concatMap reads . map revDescription $ followingRevisions

applyAnnotations :: ElmFile -> [EditUpdate] -> Maybe ElmFile
applyAnnotations elmFile' annotations =
  foldM (flip performEditAction) elmFile' (map (\(_, _, a) -> a) annotations)
