module Foo where

import Graphics.Element (..)
import Text (..)

bleep : number
bleep =
  42

double : number -> number
double x =
  x * 2

bigger : number -> number -> number
bigger first second =
  if
    first > second
  then first
  else second

main : Element
main =
  plainText "Hello, World!"
