{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Module where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LazyBS

-- elm-compiler
import qualified AST.Annotation
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General as General
import qualified AST.JSON ()
import qualified AST.Module
import qualified AST.Pattern as Pattern
import qualified AST.PrettyPrint as PP
import qualified AST.Variable as Var


type Definition =
  ( String -- name
  , Maybe String -- type
  , String -- binding
  )

data Module = Module
  { name :: AST.Module.Name
  , imports :: [String]
  , adts :: [String]
  , defs :: [Definition]
  }
  deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

fromAstFile :: LazyBS.ByteString -> Maybe AST.Module.CanonicalModule
fromAstFile astFile =
  (decode astFile :: Maybe AST.Module.CanonicalModule)

makeModule :: AST.Module.CanonicalModule -> Module
makeModule m =
  Module { name = AST.Module.names m
         , imports = []
         , adts = []
         , defs = definitions m
         }

definitions :: AST.Module.CanonicalModule -> [Definition]
definitions modoole =
  reverse . map definition $ letDefs program_
  where
    program_ = AST.Module.program (AST.Module.body modoole)

    -- Definitions are in one big 'let' block, which is in fact a list of lets.
    letDefs (AST.Annotation.A _ defs) =
      case defs of
        General.Var _ -> [] -- should be the "_save_the_environment!!!" varaible.
        General.Let [def] next -> def : letDefs next
    letDefs _ =
      error "unexpected AST structure"

definition :: Canonical.Def -> Definition
definition (Canonical.Definition (Pattern.Var varName) binding tipe) =
  (varName, tipe >>= Just . PP.renderPretty, PP.renderPretty binding)
definition _ =
  ("___not_implemented!!!", Nothing, "not implemented! (Arrowsmith.Module)")