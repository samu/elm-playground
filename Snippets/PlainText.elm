module Snippets.PlainText where
import Html exposing (..)

render snippet =
  div []
  [ text "this is just some plain text"
  , br [] []
  , text snippet.content
  ]