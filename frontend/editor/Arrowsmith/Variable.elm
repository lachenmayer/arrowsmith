module Arrowsmith.Variable where

type Home
  = BuiltIn
  | Module (List String)
  | Local

type alias Canonical =
  { home : Home
  , name : String
  }
