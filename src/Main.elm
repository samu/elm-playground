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
import Json.Decode as Json exposing ((:=))
import RestApi exposing (fetchSnippetList)

import Snippet exposing (getSnippetTypeByText, stringToSnippetType)

import Snippet.Base exposing
  ( Snippet
  , SnippetType (..)
  , initializeSnippet
  )

import SnippetList
import Snippet.Base exposing (interop)

import Utils exposing (..)

import DynamicList

-- MODEL --

type alias Model =
  { field : String
  , snippetList : SnippetList.Model
  , currentSnippetType : SnippetType
  , query : String
  }


-- UPDATE --

type Action
  = NoOp
  | UpdateField String
  | UpdateSnippets SnippetList.Action
  | ChooseSnippetType SnippetType
  | FetchSnippetList (Result Http.Error (List Snippet))
  | Search String

update : Action -> Model -> (Model, Effects Action)
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
      let (snippetList, effect) = SnippetList.update action model.snippetList
          transform effect = UpdateSnippets effect
      in ({ model | snippetList = snippetList }, Effects.map transform effect)
    ChooseSnippetType snippetType ->
      let model = { model |  currentSnippetType = snippetType }
      in (model, Effects.none)
    FetchSnippetList result ->
      let snippets = Result.withDefault [] result
          (newSnippetList, effect) = SnippetList.update (SnippetList.AddMany snippets) model.snippetList
          model = { model | snippetList = newSnippetList}
      in (model, Effects.map UpdateSnippets effect)
    Search query ->
      let model = { model | query = query }
      in (model, Effects.none)


-- VIEW --

view : Signal.Address Action -> Model -> Html
view address model =
  let
    newSnippet content = initializeSnippet content model.currentSnippetType
    query = regex (".*" ++ model.query ++ ".*")
    filteredEntries = List.filter (\e -> contains query e.content) model.snippetList.entries
    snippetList = model.snippetList
    filteredSnippetList = { snippetList | entries = filteredEntries }
  in
    div []
    [ input
      [ on "input" targetValue (UpdateField >> (Signal.message address))
      , onEnter address (\text -> (UpdateSnippets (SnippetList.Add (newSnippet text))))
      ] []
    , button [ onClick address (UpdateSnippets (SnippetList.Add (newSnippet model.field))) ] [ text "yo" ]
    , dropdown address model
    , input [ on "input" targetValue  (Search >> (Signal.message address))] []
    , SnippetList.view (Signal.forwardTo address UpdateSnippets) filteredSnippetList
    ]


-- START --

emptyModel : Model
emptyModel =
  { field = ""
  , snippetList = DynamicList.initialize []
  , currentSnippetType = PlainText
  , query = ""
  }

app =
  StartApp.start
    { init = (emptyModel, Effects.map FetchSnippetList fetchSnippetList)
    , inputs = []
    , update = update
    , view = view
    }

main =
  app.html


-- PORTS --

port embedSoundCloud : Signal (String, String)
port embedSoundCloud = interop.signal

port runner : Signal (Task.Task Effects.Never ())
port runner =
  app.tasks


-- DROPDOWN --

snippetTypes : List String
snippetTypes = List.map toString [PlainText, SoundCloud]

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

dropdown address model =
  select
    [ value "Sticky note"
    , on "change" targetValue (Signal.message address << ChooseSnippetType << stringToSnippetType) ]
    (snippetTypeOptions model.currentSnippetType)
