module SnippetList where
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import DynamicList exposing (DynamicList, Action, Indexed, removeButton, action)
import Snippet exposing (Snippet)
import Effects exposing (Effects)
import Task

type alias Model =
  { currentId : Int
  , entries : List Snippet
  }

type Action
  = NoOp
  | Add Snippet
  | Delete Int
  | Update Snippet Snippet.Action
  | PostRender (String, String)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Add snippet ->
      let effect = invokePostRender (toString (model.currentId-1), "das")
          model = DynamicList.update (DynamicList.Add snippet) model
      in (model, effect)
    Delete id -> (DynamicList.update (DynamicList.Delete id) model, Effects.none)
    Update snippet action ->
      let newSnippet = Snippet.update action snippet
          model = { model | entries = List.map (\entry ->
            if snippet.id == entry.id
            then newSnippet
            else entry
          ) model.entries}
      in  (model, Effects.none)
    PostRender message ->
      (model, Effects.none)

invokePostRender t =
  Signal.send interop.address t
    |> Task.map (\n -> t)
    |> Task.map PostRender
    |> Effects.task

renderEntry address snippet =
  div []
  [ Snippet.view (Signal.forwardTo address (Update snippet)) snippet
  , button [ onClick address (Delete snippet.id) ] [ text "x" ]
  ]

view : Signal.Address Action -> List Snippet -> Html
view address model =
  div [] (List.map (renderEntry address) model)

interop = Signal.mailbox ("", "")
