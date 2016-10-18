import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (Form, groupTransform, polygon, collage, filled, rect, circle, move, rotate)
import Transform exposing (Transform)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Window
import String
import Mouse
import Math exposing (calculateDistance, calculateAngle, rotateAroundPoint)
import Debug exposing (log)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model =
  { width : Int, height : Int
  , mouseX : Int, mouseY : Int
  , points : List (Float, Float)
  , point : (Float, Float)
  , edges : List Int
  }

rectangle : Float -> List (Float, Float)
rectangle factor =
  [ (-1, 1), ( 1, 1) , ( 1,-1), (-1,-1) ]
  |> List.map (\(x, y) -> (x * factor, y * factor))

init : (Model, Cmd Msg)
init =
  let
    factor = 20
    model =
    { width = 500, height = 500
    , mouseX = 0, mouseY = 0
    , points = rectangle factor
    , point = (factor * -0.3, factor * 2)
    , edges = [0,1,2,3]
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

drawRectangle : Color -> Int -> Int -> Form
drawRectangle color width height =
  filled color (rect (toFloat (width)) (toFloat (height)))

drawCircle : Color -> (Float, Float) -> Float -> Form
drawCircle color (x, y) radius =
  move (x, y) (filled color (circle radius))

drawDot : Color -> (Float, Float) -> Form
drawDot color coordinate =
  drawCircle color coordinate 3

drawBackground : Model -> Int -> Form
drawBackground {width, height} padding =
  drawRectangle (rgb 200 200 200) (width-padding) (height-padding)

drawBaseShape : Model -> Form
drawBaseShape {points} =
  filled (rgb 255 0 0) (polygon points)

screenCoordsToCollage : Int -> Int -> Float
screenCoordsToCollage screenCoord screenSize =
  (toFloat screenCoord) - ((toFloat screenSize) / 2)

buildTree : Int -> Form -> Transform -> Transform -> List Form
buildTree n form transformationMatrix previousMatrix =
  let
    newMatrix = Transform.multiply previousMatrix transformationMatrix
    form' = groupTransform newMatrix [form]
  in
    if n > 0
      then
        [form'] ++ buildTree (n-1) form transformationMatrix newMatrix
      else
        [form']

view : Model -> Html Msg
view model =
  let
    {width, height, mouseX, mouseY, point} = model
    posX = screenCoordsToCollage mouseX width
    posY = screenCoordsToCollage mouseY height

    m1 = Transform.translation -20 20
    m2 = Transform.rotation (degrees -22)
    m3 = Transform.scale 0.8
    m4 = Transform.translation 20 -20
    m5 = Transform.translation 0 40

    mx4 = List.foldl Transform.multiply Transform.identity [m1, m2, m3, m4, m5]

    -- mx5 = Transform.multiply mx4 mx4
    -- mx6 = Transform.multiply mx5 mx4
    -- mx7 = Transform.multiply mx6 mx4

    -- form1 = drawRectangle (rgb 0 0 255) 40 40
    form1 = drawBaseShape model

    -- form4 = groupTransform mx4 [form1]
    -- form5 = groupTransform mx5 [form1]
    -- form6 = groupTransform mx6 [form1]
    -- form7 = groupTransform mx7 [form1]

    forms =
      [drawBackground model 0]
      -- ++ [form1, form4, form5, form6, form7]
      ++ [form1]
      ++ buildTree 5 form1 mx4 Transform.identity

    d = Debug.log "distance" (calculateDistance (-10, 0) (10, 0))
    a = Debug.log "angle" (radians (calculateAngle (0, 1) (0, 0) (1, 0)))
    degraas = Debug.log "degrees" (a * (180 / pi))
  in
    collage width height forms |> toHtml
