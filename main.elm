import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
-- import Graphics.Input exposing (..)
import Signal exposing (..)

import Graphics.Input exposing (dropDown)
import Json.Decode exposing (customDecoder, object2, Decoder)
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
  , kind : SnippetType
  , tags : List String
  }

type Action
  = NoOp
  | UpdateField String
  | AddEntry String
  | DeleteEntry Int
  | ChooseSnippetType SnippetType
  | AddTag Int String

type SnippetType
  = PlainText
  | StickyNote
  | Markdown
  | SoundCloud

makeEntry field currentIndex currentSnippetType =
  { content = field
  , index = currentIndex
  , kind = currentSnippetType
  , tags = [] }

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
    ((regex, kind) :: t) ->
      if contains regex query
      then kind
      else findFirstMatch query t
    [] -> PlainText

figureOutSnippetType : Model -> Model
figureOutSnippetType model =
  let kind = findFirstMatch model.field regexToSnippetType
  in { model | currentSnippetType = kind }

update action model =
  case action of
    NoOp -> model
    UpdateField newValue ->
      model
        |> doUpdateField newValue
        |> figureOutSnippetType
    AddEntry str ->
      { model
        | entries = model.entries ++ [makeEntry str model.currentIndex model.currentSnippetType]
        , currentIndex = model.currentIndex + 1
      }
    DeleteEntry index ->
      { model | entries = List.filter (\t -> t.index /= index) model.entries }
    ChooseSnippetType kind ->
      { model |  currentSnippetType = kind }
    AddTag index str ->
      { model | entries = List.map (\entry ->
        if index == entry.index
        then { entry | tags = entry.tags ++ [str] }
        else entry
        ) model.entries
      }

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

renderEntry : Snippet -> Html
renderEntry snippet =
  div []
  [ text snippet.content
  , text (renderSnippetType snippet)
  , button [ onClick mainMailbox.address (DeleteEntry snippet.index) ] []
  , input
    [ onEnter (AddTag snippet.index)] []
  , div [] (List.map text (snippet.tags))
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

onEnter action =
  on "keydown"
    valueAndKeyCode
    (\v -> Signal.message mainMailbox.address (action v))

valueAndKeyCode =
  object2 (\a b -> b) (customDecoder keyCode is13) targetValue

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
