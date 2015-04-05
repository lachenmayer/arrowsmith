module Arrowsmith.ElmFile where

--import qualified Data.ByteString as BS
import Control.Monad (when)
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Either (isLeft)
import Data.Functor ((<$>))
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.FilePath.Posix (dropExtension, splitDirectories)
import System.IO (hGetContents)
import System.IO.Error (tryIOError)

import Elm.Package.Description (Description, sourceDirs)

import Arrowsmith.Module
import Arrowsmith.Paths
import Arrowsmith.Repo
import Arrowsmith.Types
import Arrowsmith.Util

elmFile :: RepoInfo -> Description -> FilePath -> IO (Maybe ElmFile)
elmFile repoInfo' description' filePath' = do
  let fullPath' = repoPath repoInfo' </> filePath'
  exists <- doesFileExist fullPath'
  return $ if exists
    then do
      return ElmFile
        { filePath = filePath'
        , fileName = fileNameFromPath (sourceDirs description') filePath'
        , compiledCode = Nothing
        , lastCompiled = Nothing
        , modul = Nothing
        , inRepo = repoInfo'
        }
    else
      Nothing

compile :: ElmFile -> IO (Either String ElmFile)
compile elmFile' = do
  let repoInfo' = inRepo elmFile'
  let filePath' = filePath elmFile'

  repo <- getRepo repoInfo'
  case repo of
    Left _ -> return $ Left "repo doesn't exist. (compile)"
    Right repo' -> do
      revision <- latest repo' filePath'
      let tempPath = backend repoInfo' </> user repoInfo' </> project repoInfo' </> revision
      tempDirectory <- temporaryDirectory tempPath

      let projectRoot = repoPath repoInfo'
      let inFile = projectRoot </> filePath'
      let outFile = tempDirectory </> filePath' <.> ".js"
      let compilerFlags = [inFile, "--yes", "--output", outFile]
      (compilerErr, exitCode) <- runCommand projectRoot compilerPath compilerFlags

      case exitCode of
        ExitSuccess -> do
          compiledCode' <- readFile outFile
          astFile <- getAstFile elmFile'
          case astFile of
            Right astFile' -> do
              source <- readFile inFile
              let modul' = moduleSourceDefs source <$> fromAstFile astFile'
              let newFile = elmFile' { compiledCode = Just compiledCode'
                                     , lastCompiled = Just revision
                                     , modul = modul'
                                     }
              return $ Right newFile
            Left err ->
              return . Left $ "ast file could not be loaded: " ++ err
        ExitFailure _ -> do
          err <- hGetContents compilerErr
          return $ Left (unlines . drop 2 . lines $ err)
  where

getLatest :: ElmFile -> IO (Either String ElmFile)
getLatest elmFile' = do
  repo <- getRepo (inRepo elmFile')
  case repo of
    Left _ -> return $ Left "repo doesn't exist. (getLatest)"
    Right repo' -> do
      revision <- latest repo' (filePath elmFile')
      if (lastCompiled elmFile') == Just revision then
        return $ Right elmFile'
      else do
        compile elmFile'

fullPath :: ElmFile -> FilePath
fullPath elmFile' =
  repoPath (inRepo elmFile') </> filePath elmFile'

fileNameFromPath :: [FilePath] -> FilePath -> QualifiedName
fileNameFromPath sourceDirs' filePath' =
  splitDirectories . dropExtension $ stripLongestPrefix sourceDirs' filePath'

getAstFile :: ElmFile -> IO (Either String LazyBS.ByteString)
getAstFile elmFile' = do
  let repoInfo' = inRepo elmFile'
  let repoPath' = repoPath repoInfo'
  description' <- getDescription repoPath'
  case description' of
    Left err -> return . Left $ "repo is not valid: " ++ err
    Right description'' -> do
      ast <- tryIOError . LazyBS.readFile $ repoPath' </> astPath description'' (fileName elmFile')
      return $ case ast of
        Left err -> Left $ show err
        Right astContents -> Right astContents
