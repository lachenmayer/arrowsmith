module Alias where

type alias SomeAlias =
  { foo : String
  , bar : List String
  }

type SomeADT v
  = Yeah
  | Nope String Int
  | DefinitelyNotM8 v String SomeADT