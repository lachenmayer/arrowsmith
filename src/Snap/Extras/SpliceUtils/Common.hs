module Snap.Extras.SpliceUtils.Common where

import qualified Data.Foldable             as F
import           Data.List
import           Snap
import           System.Directory.Tree
import           System.FilePath

getScripts :: MonadIO m => FilePath -> m [String]
getScripts d = do
    tree <- liftIO $ build d
    let files = F.foldMap ((:[]) . fst) $ zipPaths $ "" :/ free tree
    return $ sort $ filter visibleScripts files
  where
    visibleScripts fname =
        isSuffixOf ".js" fname && not (isPrefixOf "_" (takeFileName fname))

