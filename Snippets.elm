module Snippets where
import Regex exposing (Regex, regex, contains)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Snippets.PlainText
import Snippets.SoundCloud

import Tags

import DynamicList

type alias Snippet =
  { content : String
  , id : Int
  , kind : SnippetType
  , tags : DynamicList.DynamicList Tags.Tag
  }

type SnippetType
  = PlainText
  | StickyNote
  | Markdown
  | SoundCloud

initializeSnippet : String -> Int -> SnippetType -> Snippet
initializeSnippet content id kind =
  { content = content
  , id = id
  , kind = kind
  , tags = DynamicList.initialize []
  }

regexToSnippetType : List (Regex, SnippetType)
regexToSnippetType =
  [ (regex "^soundcloud.com", SoundCloud)
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

render : Snippet -> Html
render snippet =
  case snippet.kind of
    PlainText ->
      Snippets.PlainText.render snippet
    StickyNote ->
      text "this is sticky note"
    Markdown ->
      text "this is markdown"
    SoundCloud ->
      Snippets.SoundCloud.render snippet
