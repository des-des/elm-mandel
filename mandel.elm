import Html exposing (Html, button, div, text, span)
import Html.App exposing (program)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard
import Char
import Window
import Task

main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Vec =
  (Float, Float)

type alias Model =
  { x0: Float
  , y0: Float
  , x1: Float
  , y1: Float
  , xBound: Int
  , yBound: Int
  }

init =
  ( Model -0.0 -0.0 2.0 1.5 100 100
  , Task.perform (\_ -> NoOp) DimsMsg Window.size)

type Msg
    = KeyMsg Keyboard.KeyCode
    | DimsMsg { width: Int, height: Int }
    | NoOp

-- UPDATE


type Move
  = Up
  | Down
  | Left
  | Right
  | NoDirection

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    KeyMsg code -> (moveModel model (keyToMv code), Cmd.none)
    DimsMsg { width, height } -> ({ model | xBound = width , yBound = height }, Cmd.none)
    NoOp -> (model, Cmd.none)

stepDist: (Int, Int) -> Vec -> Vec -> Vec
stepDist (w, h) (x0, y0) (x1, y1) =
  let
    (xd, yd) = (x1 - x0, y1 - y0)
  in
    ( xd / (toFloat (stepCount w))
    , yd / (toFloat (stepCount h))
    )

moveModel: Model -> Move -> Model
moveModel model action =
  let
    (xd, yd) =
      stepDist
        (model.xBound, model.yBound)
        (model.x0, model.y0)
        (model.x1, model.y1)
  in
    case action of
      Up ->
        { model |
          y0 = model.y0 + yd,
          y1 = model.y1 + yd
      }
      Down ->
        { model |
          y0 = model.y0 - yd,
          y1 = model.y1 - yd
      }
      Left ->
        { model |
          x0 = model.x0 - xd,
          x1 = model.x1 - xd
      }
      Right ->
        { model |
          x0 = model.x0 + xd,
          x1 = model.x1 + xd
      }
      NoDirection ->
        model

showKey keyCode =
  let
    str = Debug.log (toString (Char.fromCode keyCode))
  in
    keyCode

keyToMv keyCode =
  case (Char.fromCode (showKey keyCode)) of
    'W' -> Up
    'S' -> Down
    'A' -> Left
    'D' -> Right
    _ -> NoDirection

square: Vec -> Vec
square (x, y) =
  (x^2 - y^2, 2*x*y)

add: Vec -> Vec -> Vec
add (x1, y1) (x2, y2) =
  (x1 + x2, y1 + y2)

next: Vec -> Vec -> Vec
next last c = add (square last) c

size: Vec -> Float
size (x, y) =
  x^2 + y^2

getIterationsIt: Vec -> Vec -> Int -> Int -> Int
getIterationsIt x c iteration maxIterations =
  if iteration == maxIterations || size x > 2 then
    iteration
  else
    getIterationsIt (next x c) c (iteration + 1) maxIterations

getIterations: Vec -> Int -> Int
getIterations c maxIterations =
  getIterationsIt (0, 0) c 0 maxIterations

mandelIterations: (Int, Int) -> (Int, Int) -> Vec -> Vec -> Int
mandelIterations (i, j) (iN, jN) (x0, y0) (x1, y1) =
  let
    (xRatio, yRatio) = ((toFloat i) / (toFloat iN), (toFloat j) / (toFloat jN))
    (x, y) = (x0 + (x1 - x0)*xRatio, y0 + (y1 - y0)*yRatio)
  in
    getIterations (x, y) 100


stepCount: Int -> Int
stepCount bound =
  floor (toFloat bound / 10)

map2D: (a -> b) -> List (List a) -> List (List b)
map2D mapper mandel =
  List.map
    (\row -> (List.map (\point -> mapper point)) row)
    mandel

create2D: (Int, Int) -> List (List (Int, Int))
create2D (xN, yN) =
    List.map
      (\y -> List.map (\x -> (x, y)) [0..xN])
      [0..yN]

mandelbrot: Vec -> Vec -> (Int, Int) -> List (List Int)
mandelbrot (x0, y1) (x1, y0) (w, h) =
  let
    (xSteps, ySteps) = (stepCount w, stepCount h)
  in
    map2D
      (\(xN, yN) -> mandelIterations (xN, yN) (xSteps, ySteps) (x0, y0) (x1, y1))
      (create2D (xSteps, ySteps))

-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style
      [ ("font-family", "Courier")
      , ("background-color", "black")
      , ("color", "white")
      , ("font-size", "10px")
      -- , ("font-spacing", "0")
      -- , ("line-height", "0.8")
      ]
    ]
    (List.map
      viewRow
      (mandelbrot
        (model.x0, model.y0)
        (model.x1, model.y1)
        (model.xBound, model.yBound))
    )

-- viewRow: List Int -> Html
viewRow row =
  div
    [ style
      [ ("min-height", "9.6px")
      , ("max-height", "9.6px")
      ]
    ]
    (List.map viewPixel row)

-- viewPixel: Int -> Html
pixelWrapper: String -> String -> String -> Html Msg
pixelWrapper w h pixel =
  span [ style
    [ ("display", "inline-block")
    , ("min-width", w)
    , ("max-width", w)
    , ("min-height", h)
    , ("max-height", h)]]
  [ text pixel ]

viewPixel pixel =
  pixelWrapper "9.6px" "9.6px" (
    if pixel > 99 then
      "#"
    else if pixel > 15 then
      "X"
    else if pixel > 7 then
      "+"
    else if pixel > 2 then
      "~"
    else
      "`"
  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg,
          Window.resizes DimsMsg
        ]
