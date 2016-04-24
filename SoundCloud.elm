module SoundCloud where
import Native.SoundCloud
import Html exposing (Html, text)

bla = "blubb"

test : Int -> String -> String
test id url = Native.SoundCloud.test { id = id, url = url }

customNode : Html
customNode =
  let result = (Native.SoundCloud.customNode 0)
  in  text ""
