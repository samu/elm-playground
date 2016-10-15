import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String

main =
  beginnerProgram { model = "bla", view = view, update = update }

type alias Model = String

type Message = Update Model

update : Message -> Model -> Model
update (Update string) oldString =
  oldString

view : Model -> Html Message
view string =
  text (string)
