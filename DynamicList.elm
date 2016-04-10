module DynamicList where

import Html exposing (Html, div, span, text, br, a, input)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import List exposing (filter)
import Debug


-- MODEL --

type alias Entry a = { id : Int, item : a }

type alias DynamicList a =
  { currentId : Int
  , entries : List (Entry a)
  }

initialize : List (Entry a) -> DynamicList a
initialize list =
  { currentId = 0
  , entries = list
  }


-- UPDATE --

type Action a
  = NoOp
  | Add a
  | Delete Int

update : Action a -> DynamicList a -> DynamicList a
update action model =
  case action of
    NoOp -> model
    Add entry ->
      let updateCurrentId model = { model | currentId = model.currentId + 1 }
          createEntry entry = { id = model.currentId, item = entry }
          addEntry model = { model | entries = model.entries ++ [createEntry entry]}
      in  model |> updateCurrentId |> addEntry
    Delete id ->
      let model = { model | entries = filter (\n -> n.id /= id) model.entries }
      in  model


-- VIEW --

view : Signal.Address (Action String) -> DynamicList String -> Html
view address model =
  let renderTag address entry =
        span
          [ class "label label-info" ]
          [ text entry.item, removeButton (action entry address) ]
  in div [] (List.map (renderTag address) model.entries)


-- HELPERS --

action : Entry a -> Signal.Address (Action a) -> Html.Attribute
action entry address = onClick address (Delete entry.id)

removeIcon : Html
removeIcon = span [ class "glyphicon glyphicon-remove" ] []

removeButton : Html.Attribute -> Html
removeButton action = a [ href "#", action ] [ removeIcon ]


-- START --

main =
  Signal.map (view mailbox.address) model

defaultData : DynamicList String
defaultData =
  { currentId = 0
  , entries =  [ { id = 0, item = "one"}
               , { id = 1, item = "two"}
               ]
  }

model = Signal.foldp update defaultData mailbox.signal

mailbox = Signal.mailbox NoOp
