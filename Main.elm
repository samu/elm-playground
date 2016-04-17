-- run it: elm-live Main.elm --output elm.js
module Main where
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Tags exposing (Tag)
import DynamicList
import Task
import Effects exposing (Effects)
import StartApp
import Graphics.Input exposing (dropDown)
import Regex exposing (..)
import Http
import Time

import Snippet exposing
  ( Snippet
  , SnippetType (..)
  , getSnippetTypeByText
  , initializeSnippet
  )

import SnippetList

import Utils exposing (..)

import DynamicList

-- MODEL --

type alias ID = Int

type alias Model =
  { field : String
  -- , entries : List Snippet
  , snippetList : SnippetList.Model
  , currentId : ID
  , currentSnippetType : SnippetType
  , query : String
  }


  -- UPDATE --

type Action
  = NoOp
  | UpdateField String
  | UpdateSnippets SnippetList.Action
  | ChooseSnippetType SnippetType
  | PostRender (String, String)
  | ApiCall (Result Http.Error String)
  | Search String

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
    UpdateSnippets action ->
      let snippetList = SnippetList.update action model.snippetList
      in ({ model | snippetList = snippetList }, Effects.none)
    -- AddEntry str ->
    --   let newSnippet = initializeSnippet str model.currentId model.currentSnippetType
    --       newSnippetList = SnippetList.update (SnippetList.Add newSnippet) model.snippetList
    --   in ({ model | snippetList = newSnippetList }, invokePostRender (toString newSnippet.id, "das"))
    ChooseSnippetType snippetType ->
      let model = { model |  currentSnippetType = snippetType }
      in (model, Effects.none)
    PostRender message ->
      (model, Effects.none)
    ApiCall result ->
      let message = Result.withDefault "" result
      in (model, Effects.none)
    Search query ->
      let model = { model | query = query }
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
    newSnippet content = initializeSnippet content model.currentId model.currentSnippetType
    query = regex (".*" ++ model.query ++ ".*")
    filteredEntries = List.filter (\e -> contains query e.content) model.snippetList.entries
  in
    div []
    [ input
      [ on "input" targetValue (UpdateField >> (Signal.message address))
      , onEnter address (\text -> (UpdateSnippets (SnippetList.Add (newSnippet text))))
      ] []
    , button [ onClick address (UpdateSnippets (SnippetList.Add (newSnippet model.field))) ] [ text "yo" ]
    , dropdown address model
    , input [ on "input" targetValue  (Search >> (Signal.message address))] []
    , SnippetList.view (Signal.forwardTo address UpdateSnippets) model.snippetList.entries
    ]


-- START --

emptyModel : Model
emptyModel =
  { field = ""
  , snippetList = DynamicList.initialize []
  , currentId = 0
  , currentSnippetType = PlainText
  , query = ""
  }

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
