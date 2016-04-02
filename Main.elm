import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Tags exposing (renderTagList)

import Graphics.Input exposing (dropDown)
import Regex exposing (..)

import Snippets exposing
  ( Snippet
  , SnippetType (..)
  , getSnippetTypeByText
  , initializeSnippet
  )

import Utils exposing (..)

type alias Model =
  { field : String
  , entries : List Snippet
  , currentIndex : Int
  , currentSnippetType : SnippetType
  }

type Action
  = NoOp
  | UpdateField String
  | AddEntry String
  | DeleteEntry Int
  | ChooseSnippetType SnippetType
  | AddTag Int String

doUpdateField newValue model =
  { model | field = newValue }

doUpdateSnippetType : Model -> Model
doUpdateSnippetType model =
  let kind = getSnippetTypeByText model.field
  in { model | currentSnippetType = kind }

update action model =
  case action of
    NoOp -> model
    UpdateField newValue ->
      model
        |> doUpdateField newValue
        |> doUpdateSnippetType
    AddEntry str ->
      { model
        | entries = model.entries ++ [initializeSnippet str model.currentIndex model.currentSnippetType]
        , currentIndex = model.currentIndex + 1
      }
    DeleteEntry index ->
      { model | entries = List.filter (\t -> t.index /= index) model.entries }
    ChooseSnippetType kind ->
      { model |  currentSnippetType = kind }
    AddTag index str ->
      { model | entries = List.map (\entry ->
        if index == entry.index
        then { entry | tags = Tags.update (Tags.Add str) entry.tags }
        else entry
        ) model.entries
      }

renderEntry : Snippet -> Html
renderEntry snippet =
  div []
  [ Snippets.render snippet
  , button [ onClick mainMailbox.address (DeleteEntry snippet.index) ] []
  , input
    [ onEnter mainMailbox.address (AddTag snippet.index)] []
  , renderTagList snippet.tags
  ]

sendMessage a =
  Signal.message mainMailbox.address (UpdateField a)

snippetTypes : List String
snippetTypes = List.map toString [PlainText, StickyNote, Markdown, SoundCloud]

snippetTypeOptions : SnippetType -> List Html
snippetTypeOptions currentSnippetType =
  let
    typeAsString = toString currentSnippetType
    bla = Debug.watch "typeAsString" typeAsString
  in
    List.map (\item ->
      option
      [ if typeAsString == item then selected True else selected False]
      [text item]
    ) snippetTypes

doChooseSnippetType a =
  let
    kind = case a of
      "PlainText" -> PlainText
      "Sticky note" -> StickyNote
      "Markdown" -> Markdown
      "SoundCloud" -> SoundCloud
      _ -> PlainText
  in
    Signal.message mainMailbox.address (ChooseSnippetType kind)

view : Model -> Html
view model =
  div []
  [ input
    [ on "input" targetValue sendMessage
    , onEnter mainMailbox.address AddEntry
    ] []
  , button
    [ onClick mainMailbox.address (AddEntry model.field) ] []
  , select
    [ value "Sticky note"
    , on "change" targetValue doChooseSnippetType ]
    (snippetTypeOptions model.currentSnippetType)
  , div [] (List.map renderEntry model.entries)
  ]

emptyModel =
  { field = "", entries = [], currentIndex = 0, currentSnippetType = PlainText }

mainMailbox = Signal.mailbox NoOp
modelSignal = Signal.foldp update emptyModel mainMailbox.signal

main =
  Signal.map view modelSignal
