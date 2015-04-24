module Foo where

import Graphics.Element exposing (..)
import Text exposing (..)

modulus : number
modulus =
  bigger 103 107

encrypt : number -> number -> number
encrypt m e =
  m ^ e % modulus

--bigger : number -> number -> number
bigger first second =if
    first > second
  then first
  else second

main : Element
main =
  show "Hello, World!"
