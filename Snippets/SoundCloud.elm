module Snippets.SoundCloud where
import Html exposing (..)
import Html.Attributes exposing (..)

render snippet =
  div [ id (toString snippet.id) ]
  [ text "this is soundcloud yeah!"
  , br [] []
  , text snippet.content
  ]
