module Snippet.PlainText where
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onBlur, targetValue, on)
import Snippet.Base exposing (Action(..), Content)
import Signal

renderContent address content isEdited =
  case isEdited of
    True ->
      div []
        [ input [ value content, on "input" targetValue (Update >> (Signal.message address)) ] []
        , button [ onClick address (IsEditing False) ] [ text "save" ] ]
    False ->
      div []
        [ text content
        , button [ onClick address (IsEditing True) ] [ text "edit" ]
        ]

render : Signal.Address Action -> { a | content : Content, id : Int, isEdited : Bool } -> Html
render address {content, id, isEdited} =
  div []
  [ text ("this is just some plain text" ++ (toString id))
  , br [] []
  , renderContent address content isEdited
  ]
