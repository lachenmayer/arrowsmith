module Foo where

import Graphics.Element (..)
import Text (..)

bleep : number
bleep =
  42

double : number -> number
double x =
  x * 2

main : Element
main =
  plainText "Hello, World!"
