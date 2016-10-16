import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (Form, polygon, collage, filled, rect, move)
import Color exposing (rgb)
import Element exposing (toHtml)
import Window
import String
import Mouse

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model =
  { width : Int, height : Int
  , mouseX : Int, mouseY : Int
  , points : List (Float, Float)
  }

rectangle : Float -> List (Float, Float)
rectangle factor =
  [ (-1, 1), ( 1, 1) , ( 1,-1), (-1,-1) ]
  |> List.map (\(x, y) -> (x * factor, y * factor))

init : (Model, Cmd Msg)
init =
  let model =
    { width = 500, height = 500
    , mouseX = 0, mouseY = 0
    , points = rectangle 10
    }
  in (model, Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\{height, width} -> Resize width height)
    , Mouse.moves (\{x, y} -> MouseMove x y)
    ]

type Msg
  = Update
  | Resize Int Int
  | MouseMove Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update -> (model, Cmd.none)
    Resize width height ->
      ({model | width = width, height = height}, Cmd.none)
    MouseMove x y ->
      ({model | mouseX = x, mouseY = y}, Cmd.none)

drawBackground : Model -> Int -> Form
drawBackground {width, height} padding =
  filled (rgb 200 200 200) (rect (toFloat (width-padding)) (toFloat (height-padding)))

drawBaseShape : Model -> Form
drawBaseShape {points} =
  filled (rgb 255 0 0) (polygon points)

screenCoordsToCollage : Int -> Int -> Float
screenCoordsToCollage screenCoord screenSize =
  (toFloat screenCoord) - ((toFloat screenSize) / 2)

view : Model -> Html Msg
view model =
  let
    {width, height, mouseX, mouseY} = model
    posX = screenCoordsToCollage mouseX width
    posY = screenCoordsToCollage mouseY height
    forms =
      [drawBackground model 0]
      -- ++ [move (posX, -posY) (filled (rgb 1 1 1) (rect 10 10))]
      ++ [move (posX, -posY) (drawBaseShape model)]
  in
    collage width height forms |> toHtml
