module SnippetList where
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import DynamicList exposing (DynamicList, Action, Indexed, removeButton, action)
import Snippet exposing (Snippet)

type alias Model =
  { currentId : Int
  , entries : List Snippet
  }

type Action
  = NoOp
  | Add Snippet
  | Delete Int
  | Update Snippet Snippet.Action

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Add snippet -> DynamicList.update (DynamicList.Add snippet) model
    Delete id -> DynamicList.update (DynamicList.Delete id) model
    Update snippet action ->
      let newSnippet = Snippet.update action snippet
          model = { model | entries = List.map (\entry ->
            if snippet.id == entry.id
            then newSnippet
            else entry
          ) model.entries}
      in  model

renderEntry address snippet =
  div []
  [ Snippet.view (Signal.forwardTo address (Update snippet)) snippet
  , button [ onClick address (Delete snippet.id) ] [ text "x" ]
  ]

view : Signal.Address Action -> List Snippet -> Html
view address model =
  div [] (List.map (renderEntry address) model)
