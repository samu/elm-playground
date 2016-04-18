module Snippet.Base where
import DynamicList exposing (Indexed)
import Tags exposing (Tag)
import Effects exposing (Effects)
import Task


-- MODEL --

type alias Content = String

type alias Snippet = Indexed
  { content : Content
  , kind : SnippetType
  , tags : List Tag
  , currentTagId : Int
  , isEdited : Bool
  }

type SnippetType
  = PlainText
  | SoundCloud

initializeSnippet : String -> SnippetType -> Snippet
initializeSnippet content kind =
  { content = content
  , id = 0
  , kind = kind
  , tags = []
  , currentTagId = 0
  , isEdited = False
  }


-- UPDATE --

type Action
  = Edit Snippet
  | Update String
  | IsEditing Bool
  | UpdateTags (DynamicList.Action Tag)
  | PostRender (String, String)

update : Action -> Snippet -> Snippet
update action model =
  case action of
    Edit snippet -> snippet
    Update text -> { model | content = text }
    IsEditing bool ->
      { model | isEdited = bool}
    UpdateTags action ->
      let dynamicList = DynamicList.update action { currentId = model.currentTagId, entries = model.tags }
          model = { model | currentTagId = dynamicList.currentId, tags = dynamicList.entries}
      in  model
    PostRender message -> model

invokePostRender t =
  Signal.send interop.address t
    |> Task.map (\n -> t)
    |> Task.map PostRender
    |> Effects.task

interop = Signal.mailbox ("", "")
