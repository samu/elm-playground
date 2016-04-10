module Tags where

import Html exposing (Html, text)
import DynamicList exposing (Entry)

type alias Tag =
  { id : Int
  , text : String
  }

render : Entry Tag -> Html
render entry = text entry.item.text

initialize : String -> Tag
initialize text = { id = 0, text = text}
