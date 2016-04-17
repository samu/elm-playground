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
  { currentId = 0
  , entries = list
  }


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


-- START --

type alias Thing = Indexed { item : String }

view : Signal.Address (Action Thing) -> DynamicList Thing -> Html
view address model =
  let renderTag address entry =
        span
          [ class "label label-info" ]
          [ text entry.item, removeButton (action entry address) ]
  in div [] (List.map (renderTag address) model.entries)

main =
  Signal.map (view mailbox.address) model

defaultData : DynamicList Thing
defaultData = { currentId = 0 , entries = [] }

add : String -> (DynamicList Thing -> DynamicList Thing)
add name = update (Add ({ id = 0 , item = name }))

doStuff : DynamicList Thing -> DynamicList Thing
doStuff data =
  data |> add "hey" |> add "yes" |> add "it" |> add "works"

model = Signal.foldp update (doStuff defaultData) mailbox.signal

mailbox = Signal.mailbox NoOp
