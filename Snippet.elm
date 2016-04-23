module Snippet where
import Regex exposing (Regex, regex, contains)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snippet.PlainText
import Snippet.SoundCloud
import DynamicList
import Utils exposing (onEnter)
import Snippet.Base exposing (Snippet, Action(..), SnippetType(..))
import Tags exposing (Tag)
import Effects


-- HELPERS --

regexToSnippetType : List (Regex, SnippetType)
regexToSnippetType =
  [ (regex "^https://soundcloud.com", SoundCloud)
  , (regex "^.*", PlainText)
  ]

getSnippetTypeByText query =
  getSnippetTypeByText' query regexToSnippetType

getSnippetTypeByText' : String -> List (Regex, SnippetType) -> SnippetType
getSnippetTypeByText' query list =
  case list of
    ((regex, kind) :: t) ->
      if contains regex query
      then kind
      else getSnippetTypeByText' query t
    [] -> PlainText


-- VIEW --

render : Signal.Address Action -> Snippet -> Html
render address snippet =
  case snippet.kind of
    PlainText -> Snippet.PlainText.render address snippet
    SoundCloud -> Snippet.SoundCloud.render address snippet

getPostEffect : Snippet.Base.Content -> SnippetType -> Int -> Effects.Effects Snippet.Base.Action
getPostEffect content kind id =
  case kind of
    PlainText -> Effects.none
    SoundCloud -> Snippet.Base.invokePostRender (toString (id), content)

view : Signal.Address Action -> Snippet -> Html
view address snippet =
  div []
  [ render address snippet
  , input [ onEnter address (\text -> (UpdateTags (DynamicList.Add (Tags.initialize text)))) ] []
  , Tags.view (Signal.forwardTo address (UpdateTags)) snippet.tags
  ]
