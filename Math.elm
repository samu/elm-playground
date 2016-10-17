module Math exposing (..)
import Collage exposing (Form, group, move, rotate, scale)

type alias Point = (Float, Float)

calculateDistance : Point -> Point -> Float
calculateDistance (from_x, from_y) (to_x, to_y) =
  (to_x - from_x) * (to_x - from_x) + (to_y - from_y) * (to_y - from_y)
  |> sqrt

calculateAngle : Point -> Point -> Point -> Float
calculateAngle p1 p2 p3 =
  let
    a = calculateDistance p1 p2
    b = calculateDistance p2 p3
    c = calculateDistance p1 p3
  in
    acos ((a * a + b * b - c * c) / (2 * a * b))

rotateAroundPoint : Float -> Point -> Form -> Form
rotateAroundPoint angle (x, y) form =
  let
    step1 = move (-x, -y) form
    step2 = rotate angle (group [step1])
    step3 = scale 0.71 step2
    step4 = move (x, y) step3
  in
    step4
    -- step2
