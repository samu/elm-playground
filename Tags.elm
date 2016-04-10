module Tags where

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)
import DynamicList exposing (DynamicList, Action, Entry, removeButton, action)

type alias Tag = { text : String }


view : Signal.Address (Action Tag) -> DynamicList Tag -> Html
view address model =
  let renderTag address entry =
        span
          [ class "label label-info" ]
          [ text entry.item.text, removeButton (action entry address) ]
  in div [] (List.map (renderTag address) model.entries)


initialize : String -> Tag
initialize text = { text = text}
