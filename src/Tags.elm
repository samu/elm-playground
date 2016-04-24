module Tags where
import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class)
import DynamicList exposing (DynamicList, Indexed, removeButton, action)


-- MODEL --

type alias Tag = Indexed { text : String }

type alias Model = DynamicList Tag


-- VIEW --

view : Signal.Address (DynamicList.Action Tag) -> List Tag -> Html
view address model =
  let renderTag address entry =
        span
          [ class "label label-info" ]
          [ text entry.text, removeButton (action entry address) ]
  in div [] (List.map (renderTag address) model)

initialize : String -> Tag
initialize text = { id = 0, text = text}
