module DynamicList where

import Html exposing (Html, div, span, text, br, a, input)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import List exposing (filter)
import Debug

type alias Entry a = { id : Int, item : a }

type alias DynamicList a =
  { currentId : Int
  , entries : List (Entry a)
  }

removeIcon : Html
removeIcon = span [ class "glyphicon glyphicon-remove" ] []

action : Entry a -> Signal.Address (Action a) -> Html.Attribute
action entry address = onClick address (Delete entry.id)

removeButton : Html.Attribute -> Html
removeButton action = a [ href "#", action ] [ removeIcon ]

renderEntryDefault : (Entry a -> Html) -> Signal.Address (Action a) -> Entry a -> Html
renderEntryDefault render address entry =
  span [ class "label label-info" ] [ render entry, removeButton (action entry address) ]

view : (Entry a -> Html) -> Signal.Address (Action a) -> DynamicList a -> Html
view render address model =
  div [] (List.map (renderEntryDefault render address) model.entries)

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


defaultData : DynamicList String
defaultData =
  { currentId = 0
  , entries =  [ { id = 0, item = "one"}
               , { id = 1, item = "two"}
               ]
  }

myRender : Entry String -> Html
myRender entry = text entry.item

main =
  Signal.map (view myRender mailbox.address) model

model = Signal.foldp update defaultData mailbox.signal

mailbox = Signal.mailbox NoOp
