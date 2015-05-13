{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import AST.JSON ()
import qualified AST.Module


-- Editor

type ElmCode = String
type ElmError = String -- TODO add ranges
type VarName = String
type Name = [String]
type Location = (Int {- line -}, Int {- column -}) -- 1-indexed
type Definition = (VarName, Maybe Type, ElmCode)
type PartialDefinition = (Maybe VarName, Maybe Type, Maybe ElmCode)
type LocatedDefinition = (VarName, Maybe Type, ElmCode, Location {- start -}, Location {- end -})
type Type = String

-- Re-implementation of elm-compiler:AST.Variable.Listing
data Listing a = Listing
  { explicits :: [a]
  , open :: Bool
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Listing)

-- Re-implementation of elm-compiler:AST.Module.ImportMethod
data ImportMethod = ImportMethod
  { alias :: Maybe VarName
  , exposedVars :: Listing String
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''ImportMethod)

type Import = (Name, ImportMethod)

data Module = Module
  { name :: Name
  , imports :: [Import]
  , types :: AST.Module.Types
  , defs :: [LocatedDefinition]
  , errors :: [ElmError]
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

data Repo = Repo { repoInfo :: RepoInfo }

data Project = Project
  { projectRepo :: RepoInfo
  , elmFiles :: HashMap Name ElmFile
  } deriving (Show, Eq)

data ElmFile = ElmFile
  { filePath :: FilePath -- relative to project root
  , fileName :: Name
  , source :: String
  , compiledCode :: Maybe String
  , modul :: Maybe Module
  , inRepo :: RepoInfo
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''ElmFile)

type EditUpdate = (Name, Maybe RevisionId, Action)

data CompileResponse
  = CompileSuccess ElmFile
  | CompileFailure ElmError
  deriving (Show)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''CompileResponse)

toCompileResponse :: Either ElmError ElmFile -> CompileResponse
toCompileResponse (Left err) = CompileFailure err
toCompileResponse (Right elmFile') = CompileSuccess elmFile'

data EditResponse
  = EditSuccess CompileResponse
  | EditFailure String
  deriving (Show)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''EditResponse)


-- Serving

type ProjectsMap = HashMap RepoInfo Project

data App = App
  { _projects :: IORef ProjectsMap
  }
makeLenses ''App

type AppHandler = Handler App App
type Route = (BS.ByteString, AppHandler ())
