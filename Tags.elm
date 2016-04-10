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

module Tags where

import DynamicList

type alias Tag =
  { id : Int
  , text : String
  }

initializeTag : Int -> String -> Tag
initializeTag id text = { id = id, text = text}
