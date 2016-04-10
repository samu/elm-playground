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

module DynamicList where

import Html exposing (Html, div, span, text, br, a, input)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import List exposing (filter)
import Debug

type alias DynamicList a =
  { currentId : Int
  , tags : List { id : Int, item : a }
  }

-- type alias StringList = DynamicList String

-- view : Signal.Address Action -> DynamicList a -> Html
-- view address model =
--   let removeIcon = span [ class "glyphicon glyphicon-remove" ] []
--       removeButton tag = a [ href "#", onClick address (Delete tag.id) ] [ removeIcon ]
--       print tag = span [ class "label label-info" ] [ text ("tag.text"), removeButton tag ]
--   in  div [] (List.map print model.tags)

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
          updateTags model = { model | tags = model.tags ++ [ { id = model.currentId, item = entry } ]}
      in  model |> updateCurrentId |> updateTags
    Delete id ->
      let model = { model | tags = filter (\n -> n.id /= id) model.tags }
      in  model


-- defaultData : DynamicList String
-- defaultData =
--   { currentId = 0
--   , tags =  [ "one"
--             , "two"
--     ]
--   }
--
-- main =
--   Signal.map (view mailbox.address) model
--
-- model = Signal.foldp update defaultData mailbox.signal
--
-- mailbox = Signal.mailbox NoOp
