module Arrowsmith.ElmFile where

--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Functor ((<$>))
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.FilePath.Posix (dropExtension, splitDirectories)
import System.IO.Error (tryIOError)

import Elm.Package.Description (Description, sourceDirs)

import Arrowsmith.Module
import Arrowsmith.Paths
import Arrowsmith.Types
import Arrowsmith.Util

elmFile :: Repo -> Description -> FilePath -> IO (Maybe ElmFile)
elmFile repo' description' filePath' = do
  let fullPath' = repoPath repo' </> filePath'
  exists <- doesFileExist fullPath'
  return $ if exists
    then do
      return ElmFile
        { filePath = filePath'
        , fileName = fileNameFromPath (sourceDirs description') filePath'
        , compiledCode = Nothing
        , lastCompiled = Nothing
        , modul = Nothing
        , inRepo = repo'
        }
    else
      Nothing

compile :: ElmFile -> IO (CompileStatus, ElmFile)
compile elmFile' = do
  let repo' = inRepo elmFile'
  let filePath' = filePath elmFile'
  revision <- latest repo' filePath'
  let tempPath = let ri = repoInfo repo'
                 in backend ri </> user ri </> project ri </> revision
  tempDirectory <- temporaryDirectory tempPath

  let projectRoot = repoPath repo'
  let inFile = projectRoot </> filePath'
  let outFile = tempDirectory </> filePath' <.> ".js"
  let compilerFlags = [inFile, "--yes", "--output", outFile]
  (compilerErr, exitCode) <- runCommand projectRoot compilerPath compilerFlags

  case exitCode of
    ExitSuccess -> do
      compiledCode' <- LazyBS.readFile outFile
      astFile <- getAstFile elmFile'
      return $ case astFile of
        Right astFile' ->
          let
            modul' = modulePrettyPrintedDefs <$> fromAstFile astFile'
            newFile = elmFile' { compiledCode = Just compiledCode'
                               , lastCompiled = Just revision
                               , modul = modul'
                               }
          in
            (CompileSuccess, newFile)
        Left err ->
          compileFailure (C8.pack ("ast file could not be loaded: " ++ err))
    ExitFailure _ -> do
      err <- LazyBS.hGetContents compilerErr
      return $ compileFailure (C8.unlines . drop 2 . C8.lines $ err)
  where
    -- A compile failure will always keep the elm file unchanged.
    compileFailure message =
      (CompileFailure message, elmFile')

fullPath :: ElmFile -> FilePath
fullPath elmFile' =
  repoPath (inRepo elmFile') </> filePath elmFile'

fileNameFromPath :: [FilePath] -> FilePath -> QualifiedName
fileNameFromPath sourceDirs' filePath' =
  splitDirectories . dropExtension $ stripLongestPrefix sourceDirs' filePath'

getAstFile :: ElmFile -> IO (Either String LazyBS.ByteString)
getAstFile elmFile' = do
  let repo' = inRepo elmFile'
  description' <- getDescription (repoPath repo')
  case description' of
    Left err -> return . Left $ "repo is not valid: " ++ err
    Right description'' -> do
      ast <- tryIOError . LazyBS.readFile $ astPath description'' (fileName elmFile')
      return $ case ast of
        Left err -> Left $ show err
        Right astContents -> Right astContents
