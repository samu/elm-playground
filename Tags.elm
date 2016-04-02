module Tags where

import Html exposing (div, text)

type alias Tag = String

renderTagList tags =
  div [] (List.map text (tags))

type Action
  = Add Tag

update action model =
  case action of
    Add tag -> model ++ [tag]
