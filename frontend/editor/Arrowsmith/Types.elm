-- This should mirror src/Arrowsmith/Types.hs
module Arrowsmith.Types where

import Graphics.Element exposing (Element)
import Dict exposing (Dict)

type alias CompileResponse = String
type alias ElmCode = String
type alias ElmError = String
type alias FilePath = String
type alias VarName = String
type alias ModuleName = List VarName
type alias ValueViews = Dict VarName ModuleName

type alias Type = String

type alias Definition = (VarName, Maybe Type, ElmCode)

type alias Alias = (List VarName, Type)

type alias Listing a =
  { explicits : List a
  , open : Bool
  }

type alias ImportMethod =
  { alias : Maybe VarName
  , exposedVars : Listing String
  }

type alias Import =
  (ModuleName, ImportMethod)

type alias AdtInfo =
  { adtVars : List VarName
  , constructors : List (String, List Type)
  }

type alias Module =
  { name : ModuleName
  , imports : List Import
  , types : List (VarName, Type)
  , datatypes : List (VarName, AdtInfo)
  , aliases : List (VarName, Alias)
  , defs : List Definition
  }

type alias Repo =
  { backend : String
  , user : String
  , project : String
  }

type alias ElmFile =
  { filePath : FilePath -- relative to project root
  , fileName : ModuleName
  , source : ElmCode
  , compiledCode : Maybe String
  , modul : Maybe Module
  , inRepo : Repo
  , errors : List ElmError
  }

type CompileStatus
  = Compiled
  | CompileError ElmError

type alias ViewInfo =
  { matches : Type -> Bool
  , name : ModuleName
  }