module Snippets.SoundCloud where
import Html exposing (..)

render snippet =
  div []
  [ text "this is soundcloud yeah!"
  , br [] []
  , text snippet.content
  ]
