module Snippet.PlainText where
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onDoubleClick, onBlur, targetValue, on)
import Snippet.Base exposing (Action(..), Content)
import Signal

renderContent address content isEdited =
  case isEdited of
    True ->
      input
        [ value content
        , onBlur address (IsEditing False)
        , on "input" targetValue (Update >> (Signal.message address)) ] []
    False -> div [ onDoubleClick address (IsEditing True) ] [ text content ]

render : Signal.Address Action -> { a | content : Content, id : Int, isEdited : Bool } -> Html
render address {content, id, isEdited} =
  div []
  [ text ("this is just some plain text" ++ (toString id))
  , br [] []
  , renderContent address content isEdited
  ]
