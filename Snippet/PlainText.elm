module Snippet.PlainText where
import Html exposing (..)

render {content} =
  div []
  [ text "this is just some plain text"
  , br [] []
  , text content
  ]
