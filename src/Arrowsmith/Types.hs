{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Arrowsmith.Types where

import Control.Lens.TH
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.Hashable
import Data.HashMap.Strict
import Data.IORef
import qualified Data.FileStore
import Data.List (intercalate)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import Snap.Snaplet (Handler)


-- Editor


data Home
  = BuiltIn
  | ModuleHome [String]
  | Local
  deriving (Eq, Ord, Show, Read)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''Home)

data CanonicalVar = CanonicalVar
  { home :: !Home
  , varName :: !String
  }
  deriving (Eq, Ord, Show, Read)
$(deriveJSON defaultOptions ''CanonicalVar)

data Type
  = Lambda Type Type
  | Var String
  | Type CanonicalVar
  | TypeApp Type [Type]
  | Record [(String, Type)] (Maybe Type)
  | Aliased CanonicalVar [(String, Type)] (AliasType)
  deriving (Eq, Ord, Show, Read)

data AliasType
  = Holey Type
  | Filled Type
  deriving (Eq, Ord, Show, Read)

$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''Type)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''AliasType)

type ElmCode = String
type ElmError = String -- TODO add ranges
type VarName = String
type ModuleName = [String]
type Location = (Int {- line -}, Int {- column -}) -- 1-indexed
type Definition = (VarName, Maybe Type, ElmCode)
type PartialDefinition = (Maybe VarName, Maybe Type, Maybe ElmCode)
type LocatedDefinition = (VarName, Maybe Type, ElmCode, Location {- start -}, Location {- end -})

nameToString :: ModuleName -> String
nameToString =
  intercalate "."

stringToName :: String -> ModuleName
stringToName =
  splitOn "."

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

type Import = (ModuleName, ImportMethod)
data AdtInfo = AdtInfo
  { adtVars :: [String]
  , constructors :: [(String, [Type])]
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''AdtInfo)

data Module = Module
  { name :: ModuleName
  , imports :: [Import]
  , types :: [(VarName, Type)]
  , datatypes :: [(VarName, AdtInfo)]
  , aliases :: [(VarName, ([VarName], Type))]
  , defs :: [LocatedDefinition]
  , errors :: [ElmError]
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

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

data ElmFile = ElmFile
  { filePath :: FilePath -- relative to project root
  , fileName :: ModuleName
  , source :: ElmCode
  , compiledCode :: Maybe String
  , modul :: Maybe Module
  , inRepo :: RepoInfo
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''ElmFile)

data Project = Project
  { projectRepo :: RepoInfo
  , elmFiles :: HashMap String ElmFile
  } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Project)

data EditAction
  = AddDefinition Definition
  | ChangeDefinition VarName ElmCode
  | RemoveDefinition VarName
  | ReplaceText ElmCode
  deriving (Show, Read, Eq)
$(deriveJSON defaultOptions { sumEncoding = TwoElemArray } ''EditAction)

type EditUpdate = (ModuleName, Maybe RevisionId, EditAction)

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
