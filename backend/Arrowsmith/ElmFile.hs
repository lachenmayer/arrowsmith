module Arrowsmith.ElmFile where

--import qualified Data.ByteString as BS
import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as LazyBS
import Data.Functor ((<$>))
import System.Directory (copyFile, doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.FilePath.Posix (addExtension, dropExtension, splitDirectories)
import System.IO.Strict (readFile, hGetContents)
import System.IO.Error (tryIOError, catchIOError)
import Text.Read (readMaybe)

import Elm.Compiler.Module (hyphenate)
import qualified Elm.Compiler.Module as Elm
import Elm.Package.Description (Description, sourceDirs)

import Arrowsmith.Edit
import Arrowsmith.Git (repoRunAtRevision)
import Arrowsmith.Module
import Arrowsmith.Paths
import Arrowsmith.Repo
import Arrowsmith.Types
import Arrowsmith.Update
import Arrowsmith.Util


elmFile :: RepoInfo -> Description -> FilePath -> IO (Maybe ElmFile)
elmFile repoInfo' description' filePath' = do
  let fullPath' = repoPath repoInfo' </> filePath'
  exists <- doesFileExist fullPath'
  if exists then do
    source' <- readFile fullPath'
    return $ Just ElmFile
      { filePath = filePath'
      , fileName = fileNameFromPath (sourceDirs description') filePath'
      , source = source'
      , compiledCode = Nothing
      , modul = Nothing
      , inRepo = repoInfo'
      , errors = []
      }
  else
    return Nothing

edit :: ElmFile -> EditAction -> IO Bool
edit elmFile' action' = do
  let filePath' = filePath elmFile'
  Right repo <- getRepo (inRepo elmFile')
  previousRevision <- latest repo filePath'
  previousEditUpdate <- getEditUpdate repo previousRevision
  case performEditAction action' elmFile' of
    Nothing -> return False
    Just updatedElmFile -> do
      save repo filePath' "editing..." (source updatedElmFile)
      editUpdate <- makeEditUpdate updatedElmFile action' previousRevision previousEditUpdate
      amendCommitMessage repo (show editUpdate)
      return True

compile :: ElmFile -> FilePath -> IO (Either String ElmFile)
compile elmFile' outPath = do
  let repoInfo' = inRepo elmFile'
  tempDirectory <- temporaryDirectory (backend repoInfo' </> user repoInfo' </> project repoInfo' </> outPath)
  let tempFile ext = tempDirectory </> hyphenate (Elm.Name (fileName elmFile')) <.> ext
      projectRoot = repoPath repoInfo'
      inFile = projectRoot </> (filePath elmFile')
      outFile = tempFile "js"
      compilerFlags = [inFile, "--yes", "--output", outFile]

  removeBuildArtifacts projectRoot (fileName elmFile')

  (_, compilerErr, exitCode) <- runCommand projectRoot compilerPath compilerFlags

  case exitCode of
    ExitSuccess -> do
      compiledCode' <- readFile outFile
      astFile <- getAstFile elmFile'
      case astFile of
        Right astFile' -> do
          Right astPath' <- getAstPath elmFile'
          copyFile astPath' (tempFile "elma")
          source' <- readFile inFile
          let modul' = moduleSourceDefs source' <$> fromAstFile astFile'
              newFile = elmFile' { compiledCode = Just compiledCode'
                                 , source = source'
                                 , modul = modul'
                                 , errors = []
                                 }
          return $ Right newFile
        Left err ->
          return . Left $ "ast file could not be loaded: " ++ err
    ExitFailure _ -> do
      err <- hGetContents compilerErr
      return $ Left (unlines . drop 2 . lines $ err)

getLatest :: ElmFile -> IO ElmFile
getLatest elmFile' = do
  Right repo <- getRepo (inRepo elmFile')
  headRev <- repoHead repo
  headFileOrErrors <- compile elmFile' headRev
  case headFileOrErrors of
    -- The file at HEAD compiles.
    Right file -> return file
    -- The file at HEAD does not compile, look at the last change made to the file.
    Left headErrors -> do
      latestRev <- latest repo (filePath elmFile')
      editUpdate <- getEditUpdate repo latestRev
      case editUpdate of
        -- The file has a valid Arrowsmith annotation.
        Just (updatedFile, lastWorking, _action) -> do
          if updatedFile /= fileName elmFile' then do
            print $ "update annotation in commit:\n"
              ++ latestRev
              ++ "\ndoes not refer to expected file:\n"
              ++ show elmFile'
            return plainTextFile
          else
            maybe (recoverLastWorking latestRev) recoverLastWorking lastWorking
        -- The file doesn't have a valid annotation.
        Nothing ->
          return plainTextFile

      where
        plainTextFile = elmFile' { modul = Nothing, errors = [headErrors] }

        recoverLastWorking :: RevisionId -> IO ElmFile
        recoverLastWorking lastWorkingRev = do
          lastWorkingFileOrErrors <- repoRunAtRevision repo lastWorkingRev (compile elmFile' lastWorkingRev)
          case lastWorkingFileOrErrors of
            -- There's nothing else we can do: the "last working" revision doesn't compile.
            Left _lastWorkingErrors -> do
              return plainTextFile
            -- We managed to compile at that revision. Let's update it to the latest state.
            Right lastWorkingFile -> do
              annotations <- getUpdateAnnotations repo lastWorkingFile lastWorkingRev
              case applyAnnotations lastWorkingFile annotations of
                Just latestElmFile ->
                  return latestElmFile { errors = [headErrors] }
                Nothing -> do
                  print "Updates applied unsuccessfully (ElmFile:getLatest)"
                  return plainTextFile

getEditUpdate :: Repo -> RevisionId -> IO (Maybe EditUpdate)
getEditUpdate repo revision = do
  message <- commitMessage repo revision
  return $ readMaybe message

makeEditUpdate :: ElmFile -> EditAction -> RevisionId -> Maybe EditUpdate -> IO EditUpdate
makeEditUpdate elmFile' action previousRevision previousEditUpdate = do
  Right repo <- getRepo (inRepo elmFile')
  latestRevision <- latest repo (filePath elmFile')
  latestFileOrError <- compile elmFile' latestRevision
  let editUpdate rev = (fileName elmFile', rev, action)
  return . editUpdate $ case latestFileOrError of
    Right _compiledFile -> Nothing
    Left _error -> case previousEditUpdate of
      Nothing -> Nothing
      -- Previous edit update compiles / "don't know"
      Just (_, Nothing, _) -> Just previousRevision
      -- Previous edit update definitely doesn't compile
      Just (_, Just lastWorking, _) -> Just lastWorking

fullPath :: ElmFile -> FilePath
fullPath elmFile' =
  repoPath (inRepo elmFile') </> filePath elmFile'

fileNameFromPath :: [FilePath] -> FilePath -> ModuleName
fileNameFromPath sourceDirs' filePath' =
  splitDirectories . dropExtension $ stripLongestPrefix sourceDirs' filePath'

sourcePath :: ElmFile -> String
sourcePath elmFile' =
  repoPath (inRepo elmFile') </> filePath elmFile'

getAstPath :: ElmFile -> IO (Either String FilePath)
getAstPath elmFile' = do
  let repoInfo' = inRepo elmFile'
  let repoPath' = repoPath repoInfo'
  description' <- getDescription repoPath'
  return $ case description' of
    Left err -> Left $ "repo is not valid: " ++ err
    Right description'' -> Right $ repoPath' </> astPath description'' (fileName elmFile')

getAstFile :: ElmFile -> IO (Either String LazyBS.ByteString)
getAstFile elmFile' = do
  path <- getAstPath elmFile'
  case path of
    Left err -> return $ Left err
    Right path' -> do
      ast <- (tryIOError . LazyBS.readFile) path'
      return $ either (Left . show) Right ast

removeBuildArtifacts :: FilePath -> ModuleName -> IO ()
removeBuildArtifacts projectRoot fileName' = do
  let fileName'' = hyphenate (Elm.Name fileName')
  stuffDirectory' <- getStuffDirectory projectRoot
  let basePath = stuffDirectory' </> fileName''
      artifacts = map (addExtension basePath) ["elmi", "elma", "elmo"]
  catchIOError (mapM_ removeFile artifacts) (\_ -> return ())