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

import Elm.Compiler.Module (Name(Name), hyphenate)
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
    then
      Just ElmFile
        { filePath = filePath'
        , fileName = fileNameFromPath (sourceDirs description') filePath'
        , compiledCode = Nothing
        , lastCompiled = Nothing
        , modul = Nothing
        , inRepo = repoInfo'
        }
    else
      Nothing

edit :: ElmFile -> ((ElmCode, Maybe Module) -> Maybe (ElmCode, Maybe Module)) -> IO (Maybe ElmFile)
edit elmFile' transform = do
  let sourcePath' = sourcePath elmFile'
  source <- readFile sourcePath'
  case transform (source, modul elmFile') of
    Nothing -> return Nothing
    Just (newSource, newModule) -> do
      writeFile sourcePath' newSource
      return $ Just elmFile' { modul = newModule }

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
      let tempFile ext = tempDirectory </> hyphenate (Name (fileName elmFile')) <.> ext

      let projectRoot = repoPath repoInfo'
      let inFile = projectRoot </> filePath'
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
                                     , lastCompiled = Just revision
                                     , modul = modul'
                                     }
              return $ Right newFile
            Left err ->
              return . Left $ "ast file could not be loaded: " ++ err
        ExitFailure _ -> do
          err <- hGetContents compilerErr
          return $ Left (unlines . drop 2 . lines $ err)

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