module SnippetList where
import Html exposing (text)
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
  -- | Edit Snippet.Action

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Add snippet -> DynamicList.update (DynamicList.Add snippet) model
    Delete id -> DynamicList.update (DynamicList.Delete id) model
    -- Edit action -> Snippet.update action
