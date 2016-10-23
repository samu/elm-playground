module Utils where
import Json.Decode exposing (customDecoder, object2, Decoder)
import Html

import Html.Events exposing (..)

onEnter address action =
  on "keydown"
    valueAndKeyCode
    (\v -> Signal.message address (action v))

valueAndKeyCode =
  object2 (\a b -> b) (customDecoder keyCode is13) targetValue

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
