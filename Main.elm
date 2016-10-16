import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (collage, filled, rect)
import Color exposing (rgb)
import Element exposing (toHtml)
import Window
import String

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model = String

init : (Model, Cmd Msg)
init =
  ("test", Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model =
  Window.resizes (\{height, width} -> Resize height width)

type Msg
  = Update Model
  | Resize Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg oldString =
  case msg of
    Update model -> (oldString, Cmd.none)
    Resize height width -> (oldString, Cmd.none)

view : Model -> Html Msg
view string =
  collage 100 100 [filled (rgb 1 1 1) (rect 10 10)]
  |> toHtml
