module Tweets where

import Graphics.Element exposing (..)
import Html as H
import Html.Attributes as A

--type alias Tweet =
--  { author : String
--  , text : String
--  }

fullTimeline =
  timelineView tweets

horseTimeline =
  userTimeline "horse_ebooks"

userTimeline : String -> Element
userTimeline user =
  timelineView (userTweets user)

timelineView : List { author : String, text : String } -> Element
timelineView tweets =
  flow down <| List.map tweetView tweets

tweetView : { author : String, text : String } -> Element
tweetView {author, text} =
  flow down <| [authorView author, textView text]

authorView : String -> Element
authorView contents =
  textbox 200 50 "#ccf" contents

textView : String -> Element
textView contents =
  textbox 800 50 "#cfc" contents

userTweets : String -> List { author : String, text : String }
userTweets user =
  List.filter (\x -> x.author == user) tweets

tweets : List { author : String, text : String }
tweets =
  [ { author="lachenmayer", text="hello world." }
  , { author="foobar", text="barbaz" }
  , { author="horse_ebooks", text="yes." }
  , { author="horse_ebooks", text="Bear Stearns Bravo" }
  , { author="horse_ebooks", text="The real goal of any talk or speech" }
  , { author="foobar", text="asdfasdf" }
  , { author="horse_ebooks", text="space in your house How to sell faster than your neighbors How to make a strategic use" }
  ]

textbox : Int -> Int -> String -> String -> Element
textbox width height background content =
  H.toElement width height <| H.div [ A.style [("background", background), ("padding", "20px")] ] [ H.text content ]
