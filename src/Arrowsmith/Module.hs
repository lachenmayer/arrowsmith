{-# LANGUAGE TemplateHaskell #-}
module Arrowsmith.Module where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LazyBS
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), (<.>))

-- elm-compiler
import qualified AST.Annotation
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.General
import qualified AST.JSON ()
import qualified AST.Module
import qualified AST.Pattern as Pattern
import qualified AST.PrettyPrint

import Arrowsmith.Repo (Repo, repoPath)

type Definition =
  ( String -- name
  , String -- binding
  , String -- type
  )

data Module = Module
  { name :: AST.Module.Name
  , imports :: [String]
  , adts :: [String]
  , defs :: [Definition]
  }
  deriving (Show, Eq)
$(deriveJSON defaultOptions ''Module)

getModule :: Repo -> String -> IO (Maybe Module)
getModule repo modul = do
  basePath <- getCurrentDirectory
  astFile <- LazyBS.readFile $ (repoPath basePath repo) </> "elm-stuff/build-artifacts/USER/PROJECT/1.0.0" </> modul <.> "elma" -- TODO properly
  return $ (fromAstFile astFile) >>= Just . makeModule

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
  map definition programDefs
  where
    AST.Annotation.A _ (AST.Expression.General.Let programDefs _) = program_
    program_ = AST.Module.program (AST.Module.body modoole)

definition :: Canonical.Def -> Definition
definition (Canonical.Definition (Pattern.Var varName) binding maybeTipe) =
  (varName, AST.PrettyPrint.renderPretty binding, tipe)
  where
    tipe = case maybeTipe of
      Just t -> AST.PrettyPrint.renderPretty t
      Nothing -> "a"
definition _ =
  ("___not_implemented!!!", "not implemented! (Arrowsmith.Module)", "nothing")