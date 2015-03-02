#!/bin/bash
cabal sandbox init
cabal sandbox add-source elm-make
cabal sandbox add-source elm-compiler
cabal sandbox install --only-dependencies