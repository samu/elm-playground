module DynamicList where

import Html exposing (Html, div, span, text, br, a, input)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import List exposing (filter)
import Debug


-- MODEL --

type alias Indexed a = { a | id : Int }

type alias DynamicList a =
  { currentId : Int
  , entries : List (Indexed a)
  }

initialize : List (Indexed a) -> DynamicList a
initialize list =
  let entries = List.indexedMap (\index entry -> { entry | id = index } ) list
  in  { currentId = List.length entries , entries = entries }


-- UPDATE --

type Action a
  = NoOp
  | Add a
  | Delete Int

update : Action (Indexed a) -> DynamicList (Indexed a) -> DynamicList (Indexed a)
update action model =
  case action of
    NoOp -> model
    Add entry ->
      let updateCurrentId model = { model | currentId = model.currentId + 1 }
          createEntry entry = { entry | id = model.currentId }
          addEntry model = { model | entries = model.entries ++ [createEntry entry]}
      in  model |> addEntry |> updateCurrentId
    Delete id ->
      let model = { model | entries = filter (\n -> n.id /= id) model.entries }
      in  model


-- HELPERS --

action : Indexed a -> Signal.Address (Action a) -> Html.Attribute
action entry address = onClick address (Delete entry.id)

removeIcon : Html
removeIcon = span [ class "glyphicon glyphicon-remove" ] []

removeButton : Html.Attribute -> Html
removeButton action = a [ href "#", action ] [ removeIcon ]

getMostRecent : DynamicList a -> Indexed a -> Indexed a
getMostRecent model defaultValue =
  getMostRecent' (model.currentId-1) model.entries defaultValue

getMostRecent' : Int -> List (Indexed a) -> Indexed a -> Indexed a
getMostRecent' id list defaultValue =
  case list of
    (head :: tail) ->
      if head.id == id
      then head
      else getMostRecent' id tail defaultValue
    [] -> defaultValue
