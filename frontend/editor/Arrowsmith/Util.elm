module Arrowsmith.Util where

import Html as H exposing (Html)
import Html.Attributes as A

visible : Bool -> H.Attribute
visible v =
  A.style [ ("visibility", if v then "visible" else "hidden") ]

div : String -> List Html -> Html
div class =
  H.div [ A.class class ]

tag : String -> List H.Attribute -> List Html -> Html
tag class attrs =
  H.div (A.class ("tag " ++ class) :: attrs)

editable : String -> List H.Attribute -> List Html -> Html
editable tagName attributes contents =
  H.node tagName (A.contenteditable True :: attributes) contents

span : String -> String -> Html
span class content =
  H.span [ A.class class ] [ H.text content ]

--lookup : a -> List (a, b) -> b
lookup x xs =
  snd << Maybe.withDefault ("","") << List.head <| List.filter (fst >> (==) x) xs

toggle : Bool -> Bool
toggle =
  xor True