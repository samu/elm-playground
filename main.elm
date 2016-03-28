import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)

main =
  Signal.map view modelSignal

type alias Model =
  { field : String
  , entries : List Snippet
  , currentIndex : Int
  }

type alias Snippet =
  { content : String
  , index : Int
  }

emptyModel =
  { field = "", entries = [], currentIndex = 0 }

type Action
  = NoOp
  | UpdateField String
  | AddEntry
  | DeleteEntry Int

mainMailbox = Signal.mailbox NoOp
modelSignal = Signal.foldp update emptyModel mainMailbox.signal

makeEntry content index =
  { content = content, index = index}

update action model =
  case action of
    NoOp -> model
    UpdateField field ->
      { model | field = field }
    AddEntry ->
      { model
        | entries = model.entries ++ [makeEntry model.field model.currentIndex]
        , currentIndex = model.currentIndex + 1
      }
    DeleteEntry index ->
      { model | entries = List.filter (\t -> t.index /= index) model.entries }

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
    , text model.field
    , div [] (List.map renderEntry model.entries)
    ]

renderEntry : Snippet -> Html
renderEntry {content, index} =
  div []
  [ text content
  , text (toString index)
  , button [ onClick mainMailbox.address (DeleteEntry index) ] []
  ]

sendMessage a =
  Signal.message mainMailbox.address (UpdateField a)
