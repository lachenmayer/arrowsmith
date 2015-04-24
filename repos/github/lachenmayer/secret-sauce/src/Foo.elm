module Foo where

import Color exposing (..)
import Graphics.Collage exposing (..)
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
  collage 300 300
    [ move (-10,0) (filled red (ngon 3 75))
    , move (50,10) (filled red (ngon 5 50))
    ]
