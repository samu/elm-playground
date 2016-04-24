module SnippetList where
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import DynamicList exposing (DynamicList, Action, Indexed, removeButton, action)
import Snippet
import Snippet.Base exposing (Snippet)
import Effects exposing (Effects)
import Task

-- MODEL --

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


-- UPDATE --

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    Add snippet ->
      let model = DynamicList.update (DynamicList.Add snippet) model
          mostRecentSnippet = DynamicList.getMostRecent model snippet
          effect = Snippet.getPostEffect mostRecentSnippet
      in (model, Effects.map (always NoOp) effect)
    AddMany snippets ->
      let reduceStep snippet (model, effects) =
            let (model, effect) = update (Add snippet) model
            in  (model, effects ++ [effect])
          (model, effects) = List.foldl reduceStep (model, []) snippets
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


-- VIEW --

renderEntry address snippet =
  div [ style [("border", "1px solid black")] ]
  [ Snippet.view (Signal.forwardTo address (Update snippet)) snippet
  , button [ onClick address (Delete snippet.id) ] [ text "x" ]
  ]

view : Signal.Address Action -> Model -> Html
view address model =
  div [] (List.map (renderEntry address) model.entries)
