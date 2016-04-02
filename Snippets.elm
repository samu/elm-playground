module Snippets where
import Regex exposing (Regex, regex, contains)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Snippets.PlainText

import Tags exposing (Tag)

type alias Snippet =
  { content : String
  , index : Int
  , kind : SnippetType
  , tags : List Tag
  }

type SnippetType
  = PlainText
  | StickyNote
  | Markdown
  | SoundCloud

initializeSnippet content index kind =
  { content = content
  , index = index
  , kind = kind
  , tags = [] }

renderSnippetType {kind} =
  case kind of
    PlainText ->
      "this is plaintext"
    StickyNote ->
      "this is sticky note"
    Markdown ->
      "this is markdown"
    SoundCloud ->
      "this is sound cloud"

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
  div []
  [ text snippet.content
  , text (renderSnippetType snippet)
  ]
