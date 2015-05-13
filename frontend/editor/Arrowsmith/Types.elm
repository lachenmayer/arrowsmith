-- This should mirror src/Arrowsmith/Types.hs
module Arrowsmith.Types where

import Dict exposing (Dict)
import Graphics.Element exposing (Element)
import Json.Decode as Json

import Arrowsmith.Variable as Var

type alias CompileResponse = String
type alias ElmCode = String
type alias ElmError = String
type alias VarName = String
type alias Name = List VarName
--type alias Value = String
type alias Values = Dict VarName String

type alias Definition = (VarName, Maybe String, ElmCode)

type alias Module =
  { name : Name
  , imports : List Import
  , types : Dict VarName (Type String)
  , defs : List Definition
  , errors : List ElmError
  }

type alias PortModule =
  { name : Name
  , imports : List Import
  , types : Json.Value
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

type alias ImportMethod =
  { alias : Maybe VarName
  , exposedVars : Listing String
  }

type alias Import =
  (Name, ImportMethod)

type AliasType var
  = Holey (Type var)
  | Filled (Type var)

type Type var
  = Lambda (Type var) (Type var)
  | Var String
  | Type var
  | App (Type var) (List (Type var))
  | Record (List (String, Type var)) (Maybe (Type var))
  | Aliased Var.Canonical (List (String, Type var)) (AliasType var)