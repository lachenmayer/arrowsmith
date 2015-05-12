-- This should mirror src/Arrowsmith/Types.hs
module Arrowsmith.Types where

import Graphics.Element exposing (Element)
import Dict exposing (Dict)

type alias CompileResponse = String
type alias ElmCode = String
type alias ElmError = String
type alias Name = String
type alias ModuleName = List Name
type alias Value = String
type alias Values = Dict Name Value
type alias Type = String -- TODO...

type alias Definition = (Name, Maybe Type, ElmCode)

type alias Module =
  { name : ModuleName
  , imports : List ElmCode
  , datatypes : List ElmCode
  , defs : List Definition
  , errors : List ElmError
  }

type CompileStatus
  = Compiled
  | CompileError ElmError

type alias Listing a =
  { explicits : List a
  , open : Bool
  }

type VarValue
  = VarValue String
  | VarAlias String
  | VarUnion String (Listing String)

type alias Import =
  { name : ModuleName
  , alias : Maybe Name
  , exposedVars : Listing String
  }