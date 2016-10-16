import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (Form, collage, filled, rect)
import Color exposing (rgb)
import Element exposing (toHtml)
import Window
import String

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model = { width : Int, height : Int }

init : (Model, Cmd Msg)
init =
  ({ width = 100, height = 100 }, Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model =
  Window.resizes (\{height, width} -> Resize width height)

type Msg
  = Update
  | Resize Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update -> (model, Cmd.none)
    Resize width height -> ({ width = width, height = height }, Cmd.none)

drawBackground : Model -> Int -> Form
drawBackground {width, height} padding =
  filled (rgb 200 200 200) (rect (toFloat (width-padding)) (toFloat (height-padding)))

view : Model -> Html Msg
view model =
  let
    {width, height} = model
    forms = [drawBackground model 20] ++ [filled (rgb 1 1 1) (rect 10 10)]
  in
    collage width height forms |> toHtml
