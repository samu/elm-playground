-- run it: elm-live Main.elm --output elm.js
module Main where
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Tags exposing (renderTagList)
import Task
import Effects
import StartApp.Simple exposing (start)
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

renderEntry address snippet =
  div []
  [ Snippets.render snippet
  , button [ onClick address (DeleteEntry snippet.index) ] []
  , input
    [ onEnter address (AddTag snippet.index)] []
  , renderTagList snippet.tags
  ]

sendMessage address a =
  Signal.message address (UpdateField a)

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

doChooseSnippetType address a =
  let
    kind = case a of
      "PlainText" -> PlainText
      "Sticky note" -> StickyNote
      "Markdown" -> Markdown
      "SoundCloud" -> SoundCloud
      _ -> PlainText
  in
    Signal.message address (ChooseSnippetType kind)

view address model =
  div []
  [ input
    [ on "input" targetValue (sendMessage address)
    , onEnter address AddEntry
    ] []
  , button [ onClick address (AddEntry model.field) ] [ text "yo" ]
  , select
    [ value "Sticky note"
    , on "change" targetValue (doChooseSnippetType address) ]
    (snippetTypeOptions model.currentSnippetType)
  , div [] (List.map (renderEntry address) model.entries)
  , button [ onClick interop.address ("bla-123", "le-url") ] [ text "do it" ]
  , div [ id "bla-123", style [("border", "solid black 1px"), ("height", "100px")] ] []
  ]

emptyModel =
  { field = "", entries = [], currentIndex = 0, currentSnippetType = PlainText }

main =
  StartApp.Simple.start { model = emptyModel, update = update, view = view }

interop = Signal.mailbox ("", "")

port embedSoundCloud : Signal (String, String)
port embedSoundCloud = interop.signal
