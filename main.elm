import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
-- import Graphics.Input exposing (..)
import Signal exposing (..)

import Graphics.Input exposing (dropDown)
import Json.Decode exposing (customDecoder)
import Regex exposing (..)

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

doUpdateField newValue model =
  { model | field = newValue }

regexToSnippetType : List (Regex, SnippetType)
regexToSnippetType =
  [ (regex "^soundcloud.com", SoundCloud)
  , (regex "^.*", PlainText)
  ]

findFirstMatch : String -> List (Regex, SnippetType) -> SnippetType
findFirstMatch query list =
  case list of
    ((regex, snippetType) :: t) ->
      if contains regex query
      then snippetType
      else findFirstMatch query t
    [] -> PlainText

figureOutSnippetType : Model -> Model
figureOutSnippetType model =
  let snippetType = findFirstMatch model.field regexToSnippetType
  in { model | currentSnippetType = snippetType }

update action model =
  case action of
    NoOp -> model
    UpdateField newValue ->
      model
        |> doUpdateField newValue
        |> figureOutSnippetType
    AddEntry ->
      { model
        | entries = model.entries ++ [makeEntry model]
        , currentIndex = model.currentIndex + 1
      }
    DeleteEntry index ->
      { model | entries = List.filter (\t -> t.index /= index) model.entries }
    ChooseSnippetType snippetType ->
      { model |  currentSnippetType = snippetType }

renderSnippetType {snippetType} =
  case snippetType of
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
    snippetType = case a of
      "PlainText" -> PlainText
      "Sticky note" -> StickyNote
      "Markdown" -> Markdown
      "SoundCloud" -> SoundCloud
      _ -> PlainText
  in
    Signal.message mainMailbox.address (ChooseSnippetType snippetType)

-- onEnter : Action -> Attribute
onEnter action =
    on "keydown"
      (customDecoder keyCode is13)
      (\_ -> Signal.message mainMailbox.address action)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

view : Model -> Html
view model =
  div []
  [ input
    [ on "input" targetValue sendMessage
    , onEnter AddEntry
    ] []
  , button
    [ onClick mainMailbox.address AddEntry ] []
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
