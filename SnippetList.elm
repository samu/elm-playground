module SnippetList where
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import DynamicList exposing (DynamicList, Action, Indexed, removeButton, action)
import Snippet
import Snippet.Base exposing (Snippet)
import Effects exposing (Effects)
import Task

type alias Model =
  { currentId : Int
  , entries : List Snippet
  }

type Action
  = NoOp
  | Add Snippet
  | AddMany (List Snippet)
  | Delete Int
  | Update Snippet Snippet.Base.Action

reduceStep : Snippet -> (Model, List (Effects Action)) -> (Model, List (Effects Action))
reduceStep snippet (model, effects) =
  let (model, effect) = update (Add snippet) model
  in  (model, effects ++ [effect])

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Add snippet ->
      let map effect = Update snippet effect
          model = DynamicList.update (DynamicList.Add snippet) model
          effect = Effects.map map (Snippet.getPostEffect snippet.kind (model.currentId - 1))
      in (model, effect)
    AddMany snippets ->
      let (model, effects) = List.foldl reduceStep (model, []) snippets
          effect = Effects.batch effects
      in  (model, effect)
    Delete id -> (DynamicList.update (DynamicList.Delete id) model, Effects.none)
    Update snippet action ->
      let newSnippet = Snippet.Base.update action snippet
          model = { model | entries = List.map (\entry ->
            if snippet.id == entry.id
            then newSnippet
            else entry
          ) model.entries}
      in  (model, Effects.none)

renderEntry address snippet =
  div [ style [("border", "1px solid black")] ]
  [ Snippet.view (Signal.forwardTo address (Update snippet)) snippet
  , button [ onClick address (Delete snippet.id) ] [ text "x" ]
  ]

view : Signal.Address Action -> List Snippet -> Html
view address model =
  div [] (List.map (renderEntry address) model)
