-- run it: elm-live Main.elm --output elm.js
module Main where
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Tags
import Task
import Effects exposing (Effects)
import StartApp
import Graphics.Input exposing (dropDown)
import Regex exposing (..)
import Http
import Time

import Snippets exposing
  ( Snippet
  , SnippetType (..)
  , getSnippetTypeByText
  , initializeSnippet
  )

import Utils exposing (..)


-- MODEL --

type alias Model =
  { field : String
  , entries : List Snippet
  , currentIndex : Int
  , currentSnippetType : SnippetType
  }


  -- UPDATE --

type alias ID = Int

type Action
  = NoOp
  | UpdateField String
  | AddEntry String
  | DeleteEntry Int
  | ChooseSnippetType SnippetType
  | UpdateTag ID Tags.Action
  | PostRender (String, String)
  | ApiCall (Result Http.Error String)

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp -> (model, Effects.none)
    UpdateField newValue ->
      let
        doUpdateField newValue model =
          { model | field = newValue }

        doUpdateSnippetType model =
          let snippetType = getSnippetTypeByText model.field
          in { model | currentSnippetType = snippetType }

        model = model
          |> doUpdateField newValue
          |> doUpdateSnippetType
      in (model, Effects.none)
    AddEntry str ->
      let
        newSnippet = initializeSnippet str model.currentIndex model.currentSnippetType
        model = { model
          | entries = model.entries ++ [newSnippet]
          , currentIndex = model.currentIndex + 1
        }
      in (model, invokePostRender (toString newSnippet.index, "das"))
    DeleteEntry index ->
      let model = { model | entries = List.filter (\t -> t.index /= index) model.entries }
      in (model, Effects.none)
    ChooseSnippetType snippetType ->
      let model = { model |  currentSnippetType = snippetType }
      in (model, Effects.none)
    UpdateTag index action ->
      let
        model = { model | entries = List.map (\entry ->
          if index == entry.index
          then { entry | tags = Tags.update action entry.tags }
          else entry
          ) model.entries
        }
      in (model, Effects.none)
    PostRender message ->
      (model, Effects.none)
    ApiCall result ->
      let message = Result.withDefault "" result
      in (model, Effects.none)


-- EFFECTS --

invokePostRender t =
  Signal.send interop.address t
    |> Task.map (\n -> t)
    |> Task.map PostRender
    |> Effects.task

invokeApiCall =
  Http.getString "http://localhost:3000/"
    |> Task.toResult
    |> Task.map ApiCall
    |> Effects.task


-- VIEW --

view : Signal.Address Action -> Model -> Html
view address model =
  let
    renderEntry address snippet =
      div []
      [ Snippets.render snippet
      , button [ onClick address (DeleteEntry snippet.index) ] []
      , input [ onEnter address (\text -> (UpdateTag snippet.index (Tags.Add text))) ] []
      , Tags.view (Signal.forwardTo address (UpdateTag snippet.index)) snippet.tags
      ]
  in
    div []
    [ input
      [ on "input" targetValue (UpdateField >> (Signal.message address))
      , onEnter address AddEntry
      ] []
    , button [ onClick address (AddEntry model.field) ] [ text "yo" ]
    , dropdown address model
    , div [] (List.map (renderEntry address) model.entries)
    ]


-- START --

emptyModel : Model
emptyModel =
  { field = "", entries = [], currentIndex = 0, currentSnippetType = PlainText }

app =
  StartApp.start
    { init = (emptyModel, invokeApiCall)
    , inputs = []
    , update = update
    , view = view
    }

main =
  app.html

interop = Signal.mailbox ("", "")


-- PORTS --

port embedSoundCloud : Signal (String, String)
port embedSoundCloud = interop.signal

port runner : Signal (Task.Task Effects.Never ())
port runner =
  app.tasks


-- DROPDOWN --

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


stringToSnippetType string =
  case string of
    "PlainText" -> PlainText
    "Sticky note" -> StickyNote
    "Markdown" -> Markdown
    "SoundCloud" -> SoundCloud
    _ -> PlainText

dropdown address model =
  select
    [ value "Sticky note"
    , on "change" targetValue (Signal.message address << ChooseSnippetType << stringToSnippetType) ]
    (snippetTypeOptions model.currentSnippetType)
