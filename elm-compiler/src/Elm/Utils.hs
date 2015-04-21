{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Utils ((|>), (<|), getAsset, run, unwrappedRun, CommandError(..)) where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)

import qualified Elm.Compiler.Version as Version


{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
-}
(|>) :: a -> (a -> b) -> b
x |> f = f x


{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis.
-}
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixr 0 <|
infixl 0 |>


-- RUN EXECUTABLES

data CommandError
    = MissingExe String
    | CommandFailed String String


{-| Run a command, throw an error if the command is not found or if something
goes wrong.
-}
run :: (MonadError String m, MonadIO m) => String -> [String] -> m String
run command args =
  do  result <- liftIO (unwrappedRun command args)
      case result of
        Right out -> return out
        Left err ->
          throwError (context (message err))
  where
    context msg =
      "failure when running:" ++ concatMap (' ':) (command:args) ++ "\n" ++ msg

    message err =
      case err of
        CommandFailed stderr stdout ->
          stdout ++ stderr
        MissingExe msg ->
          msg


unwrappedRun :: String -> [String] -> IO (Either CommandError String)
unwrappedRun command args =
  do  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
      return $
          case exitCode of
            ExitSuccess -> Right stdout
            ExitFailure code
                | code == 127  -> Left (missingExe command)  -- UNIX
                | code == 9009 -> Left (missingExe command)  -- Windows
                | otherwise    -> Left (CommandFailed stdout stderr)


missingExe :: String -> CommandError
missingExe command =
  MissingExe $
    "Could not find command '" ++ command ++ "'. Do you have it installed?\n\
    \    Can it be run from anywhere? Is it on your PATH?"



-- GET STATIC ASSETS

{-| Get the absolute path to a data file. If you install with cabal it will look
-}
getAsset :: String -> (FilePath -> IO FilePath) -> FilePath -> IO FilePath
getAsset project getDataFileName name =
  do  path <- getDataFileName name
      exists <- doesFileExist path
      if exists
        then return path
        else do
          environment <- tryIOError (getEnv "ELM_HOME")
          case environment of
            Right env ->
                return (env </> project </> name)

            Left _ ->
                fail (errorNotFound name)


errorNotFound :: FilePath -> String
errorNotFound name =
    unlines
    [ "Unable to find the ELM_HOME environment variable when searching"
    , "for the " ++ name ++ " file."
    , ""
    , "If you installed Elm Platform with the Mac or Windows installer, it looks like"
    , "ELM_HOME was not set automatically. Look up how to set environment variables"
    , "on your platform and set ELM_HOME to the directory that contains Elm's static"
    , "files:"
    , ""
    , "  * On Mac it is /usr/local/share/elm"
    , "  * On Windows it is one of the following:"
    , "      C:/Program Files/Elm Platform/" ++ Version.version ++ "/share"
    , "      C:/Program Files (x86)/Elm Platform/" ++ Version.version ++ "/share"
    , ""
    , "If it seems like a more complex issue, please report it here:"
    , "    <https://github.com/elm-lang/elm-platform/issues>"
    ]