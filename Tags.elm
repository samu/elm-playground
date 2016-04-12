module Tags where

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)
import DynamicList exposing (DynamicList, Action, Indexed, removeButton, action)

type alias Tag = Indexed { text : String }

type alias Model = DynamicList Tag

type Action = Action

update : Action -> Model -> Model
update action model = model

-- view : Signal.Address (Action Tag) -> DynamicList Tag -> Html
-- view address model =
--   let renderTag address entry =
--         span
--           [ class "label label-info" ]
--           [ text entry.item.text, removeButton (action entry address) ]
--   in div [] (List.map (renderTag address) model.entries)
--
--
-- initialize : String -> Tag
-- initialize text = { text = text}
