module Snippet.SoundCloud where
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Snippet.Base exposing (Snippet, Action)

render : Signal.Address Action -> Snippet -> Html
render address {content, id} =
  div [ Html.Attributes.id ("abc" ++ (toString id)) ]
  [ text ("this is soundcloud yeah!" ++ (toString id))
  , br [] []
  , text content
  , div [ Html.Attributes.id (toString id) ] [ text content]
  -- width="100%" height="100" scrolling="no" frameborder="no"
  , iframe [ style [ ("width", "100%"), ("height", "100px") ], src "https://w.soundcloud.com/player/?visual=true&url=https%3A%2F%2Fapi.soundcloud.com%2Ftracks%2F293&show_artwork=true&maxheight=100" ] []
  ]
