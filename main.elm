import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
-- import Graphics.Input exposing (..)
import Signal exposing (..)

import Graphics.Input exposing (dropDown)

type alias Model =
  { field : String
  , entries : List Snippet
  , currentIndex : Int
  , currentSnippetType : SnippetType
  }

type alias Snippet =
  { content : String
  , index : Int
  , snippetType : SnippetType
  }

type Action
  = NoOp
  | UpdateField String
  | AddEntry
  | DeleteEntry Int
  | ChooseSnippetType SnippetType

type SnippetType
  = PlainText
  | StickyNote
  | Markdown
  | SoundCloud

makeEntry : Model -> Snippet
makeEntry {field, currentIndex, currentSnippetType} =
  { content = field, index = currentIndex, snippetType = currentSnippetType}

update action model =
  case action of
    NoOp -> model
    UpdateField field ->
      { model | field = field }
    AddEntry ->
      { model
        | entries = model.entries ++ [makeEntry model]
        , currentIndex = model.currentIndex + 1
      }
    DeleteEntry index ->
      { model | entries = List.filter (\t -> t.index /= index) model.entries }
    ChooseSnippetType snippetType ->
      { model |  currentSnippetType = snippetType }

renderSnippetType snippet =
  case snippet.snippetType of
    PlainText ->
      "this is plaintext"
    StickyNote ->
      "this is sticky note"
    Markdown ->
      "this is markdown"
    SoundCloud ->
      "this is sound cloud"

renderEntry : Snippet -> Html
renderEntry snippet =
  div []
  [ text snippet.content
  , text (renderSnippetType snippet)
  , button [ onClick mainMailbox.address (DeleteEntry snippet.index) ] []
  ]

sendMessage a =
  Signal.message mainMailbox.address (UpdateField a)

doChooseSnippetType a =
  let
    snippetType = case a of
      "PlainText" -> PlainText
      "Sticky note" -> StickyNote
      "Markdown" -> Markdown
      "SoundCloud" -> SoundCloud
      _ -> PlainText
  in
    Signal.message mainMailbox.address (ChooseSnippetType snippetType)

view : Model -> Html
view model =
  let
    a = Debug.watch "model" model
  in
    div []
    [ input
      [ on "input" targetValue sendMessage
      ] []
    , button
      [ onClick mainMailbox.address AddEntry ] []
    , select [on "change" targetValue doChooseSnippetType ]
      [ option [] [ text "Plain text"]
      , option [] [ text "Sticky note"]
      , option [] [ text "Markdown"]
      , option [] [ text "SoundCloud"]
      ]
    , div [] (List.map renderEntry model.entries)
    ]

emptyModel =
  { field = "", entries = [], currentIndex = 0, currentSnippetType = PlainText }

mainMailbox = Signal.mailbox NoOp
modelSignal = Signal.foldp update emptyModel mainMailbox.signal

main =
  Signal.map view modelSignal
