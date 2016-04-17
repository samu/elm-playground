-- Some thoughts on how to organize the snippets code
-- - There are single snippets
-- - Every snippet can be shown on its own
-- - Every snippet has its own view and update logic
--   - view
--     - take arbitary content (json) and decode it
--     - generate the view accordingly
--     - animation loop?
--   - update
--     - react to actions that are specific to the snippet
--     - invoke post-render-effects (for example for soundcloud snippets)
--   - do these two parts (view and update) have to be aware of the data structure
--     of a snippet?
--     - view: all we need is the json structure, from that we can build the entire
--       representation. The tags are not directly related to a snippet. Neither is
--       the title or the id.
--     - update is probably a bit trickier:
--       - can updates on a snippet trigger updates on other snippets?
--       - ?
-- - Snippets can also appear in lists
--   - the list is unaware of the different snippet types
-- - Snippets have a basic structure: content, id, tags, ...?
--   - Most likely, snippet contents will be arbitrary json structures, hence every snippet
--     will have its own json decoder
-- -


module Snippet where
import Regex exposing (Regex, regex, contains)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snippet.PlainText
import Snippet.SoundCloud
import Tags exposing (Tag)
import DynamicList
import Utils exposing (onEnter)

-- MODEL --

type alias Content = String

type alias Snippet =
  { content : Content
  , id : Int
  , kind : SnippetType
  , tags : List Tag
  , currentTagId : Int
  }

type SnippetType
  = PlainText
  | StickyNote
  | Markdown
  | SoundCloud

initializeSnippet : String -> SnippetType -> Snippet
initializeSnippet content kind =
  { content = content
  , id = 0
  , kind = kind
  , tags = []
  , currentTagId = 0
  }


-- HELPERS --

regexToSnippetType : List (Regex, SnippetType)
regexToSnippetType =
  [ (regex "^soundcloud.com", SoundCloud)
  , (regex "^.*", PlainText)
  ]

getSnippetTypeByText query =
  getSnippetTypeByText' query regexToSnippetType

getSnippetTypeByText' : String -> List (Regex, SnippetType) -> SnippetType
getSnippetTypeByText' query list =
  case list of
    ((regex, kind) :: t) ->
      if contains regex query
      then kind
      else getSnippetTypeByText' query t
    [] -> PlainText

render : Snippet -> Html
render snippet =
  case snippet.kind of
    PlainText ->
      Snippet.PlainText.render snippet
    StickyNote ->
      text "this is sticky note"
    Markdown ->
      text "this is markdown"
    SoundCloud ->
      Snippet.SoundCloud.render snippet

-- UPDATE --

type Action
  = Edit Snippet
  | UpdateTags (DynamicList.Action Tag)

update : Action -> Snippet -> Snippet
update action model =
  case action of
    Edit snippet -> snippet
    UpdateTags action ->
      let dynamicList = DynamicList.update action { currentId = model.currentTagId, entries = model.tags }
          model = { model | currentTagId = dynamicList.currentId, tags = dynamicList.entries}
      in  model


-- VIEW --

view : Signal.Address Action -> Snippet -> Html
view address snippet =
  div []
  [ render snippet
  , input [ onEnter address (\text -> (UpdateTags (DynamicList.Add (Tags.initialize text)))) ] []
  , Tags.view (Signal.forwardTo address (UpdateTags)) snippet.tags
  ]
