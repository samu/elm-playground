module Snippet.PlainText where
import Html exposing (..)

render {content, id} =
  div []
  [ text ("this is just some plain text" ++ (toString id))
  , br [] []
  , text content
  ]
