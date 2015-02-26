{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.JSON where

import Control.Applicative ((<$>),empty)
import Data.Aeson
import Data.Aeson.TH (deriveJSON,defaultOptions)
import Text.PrettyPrint hiding (empty)

import qualified AST.Annotation
import qualified AST.Declaration
import qualified AST.Expression.Canonical
import qualified AST.Expression.General
import qualified AST.Literal
import qualified AST.Module
import qualified AST.Pattern
import qualified AST.Variable
import qualified AST.Type

$(deriveJSON defaultOptions ''AST.Annotation.Annotated)
$(deriveJSON defaultOptions ''AST.Annotation.Position)
$(deriveJSON defaultOptions ''AST.Annotation.Region)

$(deriveJSON defaultOptions ''AST.Declaration.Assoc)

$(deriveJSON defaultOptions ''AST.Expression.Canonical.Def)
$(deriveJSON defaultOptions ''AST.Expression.General.Expr')

$(deriveJSON defaultOptions ''AST.Literal.GLTipe)
$(deriveJSON defaultOptions ''AST.Literal.GLShaderTipe)
$(deriveJSON defaultOptions ''AST.Literal.Literal)

$(deriveJSON defaultOptions ''AST.Module.Module)
$(deriveJSON defaultOptions ''AST.Module.ImportMethod)
$(deriveJSON defaultOptions ''AST.Module.CanonicalBody)

$(deriveJSON defaultOptions ''AST.Pattern.Pattern)

$(deriveJSON defaultOptions ''AST.Variable.Canonical)
$(deriveJSON defaultOptions ''AST.Variable.Home)
$(deriveJSON defaultOptions ''AST.Variable.Value)
$(deriveJSON defaultOptions ''AST.Variable.Listing)

$(deriveJSON defaultOptions ''AST.Type.Type)

instance ToJSON Doc where
  toJSON doc = object ["doc" .= toJSON (render doc)]

instance FromJSON Doc where
  parseJSON (Object obj) = text <$> (obj .: "doc")
  parseJSON _ = empty