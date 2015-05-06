module Arrowsmith.ElmFile where

--import qualified Data.ByteString as BS
import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as LazyBS
import Data.Functor ((<$>))
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.FilePath.Posix (dropExtension, splitDirectories)
import System.IO.Strict (readFile, hGetContents)
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)

import Elm.Compiler.Module (Name(Name), hyphenate)
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
  return $ if exists
    then
      Just ElmFile
        { filePath = filePath'
        , fileName = fileNameFromPath (sourceDirs description') filePath'
        , compiledCode = Nothing
        , modul = Nothing
        , inRepo = repoInfo'
        }
    else
      Nothing

edit :: ElmFile -> Action -> IO (Maybe ElmFile)
edit elmFile' action' = do
  let filePath' = filePath elmFile'
  Right repo <- getRepo (inRepo elmFile')
  latestElmFile <- getLatest elmFile'
  case latestElmFile of
    Left err -> error err
    Right elmFile'' -> do
      latestRev <- latest repo filePath'
      source <- retrieve repo filePath' (Just latestRev)
      case performAction action' (source, modul elmFile'') of
        Nothing -> return Nothing
        Just (newSource, newModule) -> do
          let editUpdate rev = (fileName elmFile'', rev, action') :: EditUpdate
          save repo filePath' (show (editUpdate (Just latestRev))) newSource
          newRev <- latest repo filePath'
          newElmFile <- compile elmFile'' newRev
          case newElmFile of
            Left err ->
              return $ Just elmFile'' { modul = Just newModule { errors = [err] }, compiledCode = Nothing }
            Right compiledElmFile -> do
              amendCommitMessage repo (show (editUpdate Nothing)) -- The current revision is the last working.
              return $ Just compiledElmFile

compile :: ElmFile -> FilePath -> IO (Either String ElmFile)
compile elmFile' outPath = do
  let repoInfo' = inRepo elmFile'
  tempDirectory <- temporaryDirectory (backend repoInfo' </> user repoInfo' </> project repoInfo' </> outPath)
  let tempFile ext = tempDirectory </> hyphenate (Name (fileName elmFile')) <.> ext
  let projectRoot = repoPath repoInfo'
  let inFile = projectRoot </> (filePath elmFile')
  let outFile = tempFile "js"
  let compilerFlags = [inFile, "--yes", "--output", outFile]

  (_, compilerErr, exitCode) <- runCommand projectRoot compilerPath compilerFlags

  case exitCode of
    ExitSuccess -> do
      compiledCode' <- readFile outFile
      astFile <- getAstFile elmFile'
      case astFile of
        Right astFile' -> do
          Right astPath' <- getAstPath elmFile'
          copyFile astPath' (tempFile "elma")
          source <- readFile inFile
          let modul' = moduleSourceDefs source <$> fromAstFile astFile'
          let newFile = elmFile' { compiledCode = Just compiledCode'
                                 , modul = modul'
                                 }
          return $ Right newFile
        Left err ->
          return . Left $ "ast file could not be loaded: " ++ err
    ExitFailure _ -> do
      err <- hGetContents compilerErr
      return $ Left (unlines . drop 2 . lines $ err)

getLatest :: ElmFile -> IO (Either String ElmFile)
getLatest elmFile' = do
  repo <- getRepo (inRepo elmFile')
  case repo of
    Left err -> return $ Left err
    Right repo' -> do
      headRev <- repoHead repo'
      headFileOrErrors <- compile elmFile' headRev
      case headFileOrErrors of
        -- The file at HEAD compiles.
        Right file -> return $ Right file
        -- The file at HEAD does not compile, look at the last change made to the file.
        Left _headErrors -> do
          latestRev <- latest repo' (filePath elmFile')
          message <- commitMessage repo' latestRev
          case readMaybe message :: Maybe EditUpdate of
            -- The file has a valid Arrowsmith annotation.
            Just (updatedFile, lastWorking, _action) -> do
              if updatedFile /= fileName elmFile' then
                return . Left $ "update annotation in commit:\n"
                  ++ message
                  ++ "\ndoes not refer to expected file:\n"
                  ++ show elmFile'
              else
                maybe (recoverLastWorking latestRev) recoverLastWorking lastWorking
            -- The file doesn't have a valid annotation.
            Nothing ->
              recoverLastWorking latestRev
          where
            recoverLastWorking lastWorkingRev = do
              lastWorkingFileOrErrors <- repoRunAtRevision repo' lastWorkingRev (compile elmFile' lastWorkingRev)
              case lastWorkingFileOrErrors of
                Left lastWorkingErrors ->
                  return . Left $ "Last working revision "
                    ++ lastWorkingRev
                    ++ " doesn't actually compile: "
                    ++ lastWorkingErrors
                Right lastWorkingFile -> do
                  annotations <- getUpdateAnnotations repo lastWorkingFile lastWorkingRev
                  return . Right $ applyAnnotations annotations lastWorkingFile

fullPath :: ElmFile -> FilePath
fullPath elmFile' =
  repoPath (inRepo elmFile') </> filePath elmFile'

fileNameFromPath :: [FilePath] -> FilePath -> QualifiedName
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