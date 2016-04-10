-- Library to provide common functionality for taggable things.
-- First, lets explore on the meaning of tags:
-- - Tags can be added. This is done through a free text field.
--   - The free text field usually provides an autocomplete functionality of
--     some sorts.
--   - The free text field can be attached to the taggable object, meaning
--     there is one text field for every taggable, or it could be opened in
--     a dialog box or something similar.
-- - Tags have a visual representation. For example, they could be presentted
--   as a badge.
-- - Tags can be deleted. This is usually done by hitting an 'x' symbol on the
--   tag representation
-- - A single tag is most likely a tuple, containing an id and a string
-- - Usually, a taggable can tagged with several tags, hence tags will usually
--   appear in lists of multiple tags.

module NewTags where

import Html exposing (Html, div, span, text, br, a, input)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import List exposing (filter)
import Debug

-- type alias GenericTag a =
--   { id : Int
--   , text : String
--   , data : a
--   }

type alias Tag =
  { id : Int
  , text : String
  }

type alias Model =
  { currentIndex : Int
  , tags : List Tag
  }

view : Signal.Address Action -> Model -> Html
view address model =
  let removeIcon = span [ class "glyphicon glyphicon-remove" ] []
      removeButton tag = a [ href "#", onClick address (Delete tag) ] [ removeIcon ]
      print tag = span [ class "label label-info" ] [ text (tag.text ++ "  "), removeButton tag ]
  in  div [] ((List.map print (Debug.log "model" model.tags)))

type Action
  = NoOp
  | Add Tag
  | Delete Tag

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Add tag ->
      let model = { model
        | currentIndex = model.currentIndex + 1
        , tags = model.tags ++ [tag]
        }
      in  model
    Delete tag ->
      let model = { model | tags = filter (\n -> n.id /= tag.id) model.tags }
      in  model

initializeTags : Model
initializeTags =
  { currentIndex = 0, tags = []}

defaultData =
  { currentIndex = 0
  , tags =  [ {id = 0, text = "abc"}
            , {id = 1, text = "bla"}
    ]
  }

main =
  Signal.map (view mailbox.address) model

model = Signal.foldp update defaultData mailbox.signal

mailbox = Signal.mailbox NoOp
