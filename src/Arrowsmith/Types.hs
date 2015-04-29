{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Types where

import Control.Lens.TH
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.Hashable
import Data.HashMap.Strict
import Data.IORef
import qualified Data.FileStore
import GHC.Generics (Generic)
import Snap.Snaplet (Handler)
import System.Exit (ExitCode)


-- Editor

type ElmCode = String
type ElmError = String -- TODO add ranges
type VarName = String
type QualifiedName = [String]
type Location = (Int {- line -}, Int {- column -}) -- 1-indexed
type Definition = (VarName, Maybe Type, ElmCode)
type PartialDefinition = (Maybe VarName, Maybe Type, Maybe ElmCode)
type LocatedDefinition = (VarName, Maybe Type, ElmCode, Location {- start -}, Location {- end -})
type Type = String

data Module = Module
  { name :: QualifiedName
  , imports :: [String]
  , adts :: [String]
  , defs :: [LocatedDefinition]
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

data Action
  = AddDefinition Definition
  | ChangeDefinition VarName ElmCode
  | RemoveDefinition VarName
  deriving (Show, Read, Eq)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''Action)

data RepoInfo = RepoInfo
  { backend :: String
  , user :: String
  , project :: String
  } deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''RepoInfo)
instance Hashable RepoInfo

type CommitMessage = Data.FileStore.Description
type RevisionId = Data.FileStore.RevisionId

data Repo = Repo
  { repoInfo :: RepoInfo
  , index :: IO [FilePath]
  , latest :: FilePath -> IO RevisionId
  , retrieve :: FilePath -> Maybe RevisionId -> IO String
  , save :: FilePath -> CommitMessage -> String -> IO ()
  , checkout :: String -> IO ExitCode
  , repoHead :: IO RevisionId
  , repoBranch :: IO String
  , commitMessage :: RevisionId -> IO String
  }

data Project = Project
  { projectRepo :: RepoInfo
  , elmFiles :: HashMap QualifiedName ElmFile
  } deriving (Show, Eq)

data ElmFile = ElmFile
  { filePath :: FilePath -- relative to project root
  , fileName :: QualifiedName
  , compiledCode :: Maybe String
  , modul :: Maybe Module
  , inRepo :: RepoInfo
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''ElmFile)

type EditUpdate = (QualifiedName, Maybe RevisionId, Action)

data CompileResponse
  = CompileSuccess ElmFile
  | CompileFailure ElmError
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''CompileResponse)

toCompileResponse :: Either ElmError ElmFile -> CompileResponse
toCompileResponse (Left err) = CompileFailure err
toCompileResponse (Right elmFile') = CompileSuccess elmFile'

data EditResponse
  = EditSuccess CompileResponse
  | EditFailure String
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''EditResponse)


-- Serving

type ProjectsMap = HashMap RepoInfo Project

data App = App
  { _projects :: IORef ProjectsMap
  }
makeLenses ''App

type AppHandler = Handler App App
type Route = (BS.ByteString, AppHandler ())
