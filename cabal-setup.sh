#!/bin/bash
cabal sandbox init
cabal sandbox add-source elm-make
cabal sandbox add-source elm-compiler
cabal sandbox add-source snap-extras-0.9
cabal sandbox install --only-dependencies