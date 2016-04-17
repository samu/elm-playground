module Snippet.SoundCloud where
import Html exposing (..)
import Html.Attributes exposing (..)

render {content, id} =
  div [ Html.Attributes.id (toString id) ]
  [ text ("this is soundcloud yeah!" ++ (toString id))
  , br [] []
  , text content
  ]
