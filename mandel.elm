import Html exposing (Html, button, div, text, span)
import Html.App exposing (program)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard
import Char
import Window
import Task
import Dict exposing (Dict)

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

type alias IntVec =
  (Int, Int)

type alias Grid a =
  List (List a)

type alias IterationMap =
  Dict IntVec Int

type alias Model =
  { x0: Float
  , y0: Float
  , x1: Float
  , y1: Float
  , ofsetX: Int
  , ofsetY: Int
  , xBound: Int
  , yBound: Int
  , coordinateGrid: Grid IntVec
  , iterationMap: IterationMap
  }



init =( {
    x0 = -1.5,
    y0 = -1,
    x1 = 1.5,
    y1 = 1,
    ofsetX = 0,
    ofsetY = 0,
    xBound = 0,
    yBound = 0,
    coordinateGrid = [[]],
    iterationMap = Dict.empty
  },
  Task.perform (\_ -> NoOp) DimsMsg Window.size)



-- MATH


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

mapPoint: Vec -> Vec -> IntVec -> Vec
mapPoint (x0, y0) (xd, yd) (xi, yi) =
  (x0 + xd * (toFloat xi), y0 + yd * (toFloat yi))

getIterations: Vec -> Vec -> Int -> IntVec -> Int
getIterations origin d maxIterations c =
  getIterationsIt
    (0, 0)
    (mapPoint origin d c)
    0
    maxIterations



-- UPDATE


charSize = 12

type Msg
    = KeyMsg Keyboard.KeyCode
    | DimsMsg { width: Int, height: Int }
    | NoOp

type Move
  = Up
  | Down
  | Left
  | Right
  | NoDirection



createGrid: IntVec -> Grid IntVec
createGrid (xN, yN) =
  List.map
  (\y -> List.map (\x -> (x, y)) [0..xN])
  [0..yN]


mapGrid: (a -> b) -> Grid a -> Grid b
mapGrid mapper mandel =
  List.map
    (\row -> (List.map (\point -> mapper point)) row)
    mandel


createCoodinatesGrid: Vec -> Vec -> IntVec -> IntVec -> Grid IntVec
createCoodinatesGrid (x0, y1) (x1, y0) (ofsetX, ofsetY) (w, h) =
  let
    (xNTotal, yNTotal) = (stepCount w, stepCount h)

  in
    mapGrid
      (\(xN, yN) -> (xN - ofsetX, yN - ofsetY)

      )
      (createGrid (xNTotal, yNTotal))

reduceGrid: (a -> b -> b) -> b -> Grid a -> b
reduceGrid reducer init grid =
  List.foldl
    (\row -> \out -> (List.foldl reducer out row))
    init
    grid

refreashIterationMap: IterationMap -> Grid IntVec -> (IntVec -> Int) -> IterationMap
refreashIterationMap iterationMap grid getIterations =
  reduceGrid
    (\point -> \iterationMap ->
        case Dict.get point iterationMap of
          Just _ -> iterationMap

          Nothing -> Dict.insert point (Debug.log "p:" (getIterations point)) iterationMap
    )
    iterationMap
    grid



updateGridAndCache: Model -> Model
updateGridAndCache model  =
  let
    { x0, y0, x1, y1, xBound, yBound, coordinateGrid, iterationMap, ofsetX, ofsetY } = model
    newCoordinateGrid = createCoodinatesGrid (x0, y0) (x1, y1) (ofsetX, ofsetY) (xBound, yBound)
    (xNTotal, yNTotal) = (stepCount xBound, stepCount yBound)
    (xd, yd) =
      ( (x1 - x0) / toFloat xNTotal
      , (y1 - y0) / toFloat yNTotal
      )
  in
    { model |
      coordinateGrid = newCoordinateGrid,
      iterationMap = refreashIterationMap
        iterationMap
        newCoordinateGrid
        (getIterations (x0, y0) (xd, yd) 500)
    }


stepCount: Int -> Int
stepCount bound =
  floor (toFloat bound / charSize)


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
          ofsetY = model.ofsetY + 1
      }
      Down ->
        { model |
          ofsetY = model.ofsetY - 1
      }
      Left ->
        { model |
          ofsetX = model.ofsetX - 1
      }
      Right ->
        { model |
          ofsetX = model.ofsetX + 1
      }
      NoDirection ->
        model


update: Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    KeyMsg code ->
      ( updateGridAndCache(moveModel model (keyToMv code))
      , Cmd.none
      )
    DimsMsg { width, height } ->
      ( updateGridAndCache { model | xBound = width, yBound = height }
      , Cmd.none
      )
    NoOp -> (model, Cmd.none)


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


-- VIEW
iterationGrid: Grid IntVec -> IterationMap -> Grid Int
iterationGrid grid iterationMap =
  mapGrid
    (\point -> case Dict.get point iterationMap of
      Just iterationNumber -> iterationNumber

      Nothing -> -1
    )
    grid

view : Model -> Html Msg
view { xBound, yBound, coordinateGrid, iterationMap } =
  div
    [ style
      [ ("font-family", "Courier")
      , ("background-color", "black")
      , ("color", "white")
      , ("font-size", pxString charSize)
      , ("position", "absolute")
      , ("top", "0")
      , ("left", "0")
      , ("bottom", "0")
      , ("right", "0")
      , ("overflow", "hidden")
      ]
    ]
    (List.map
      (viewRow (xBound, yBound))
      (iterationGrid coordinateGrid iterationMap)
    )

pxString: Float -> String
pxString x =
  toString x ++ "px"

distToStr: Int -> String
distToStr bound =
  pxString (toFloat bound / (toFloat (stepCount bound)))

viewRow: (Int, Int) -> List Int -> Html Msg
viewRow (xBound, yBound) row =
  let
    (w, h) = (distToStr xBound, distToStr yBound)
  in
    div
      [ style
        [ ("min-height", h)
        , ("max-height", h)
        , ("overflow", "hidden")
        ]
      ]
      (List.map (viewPixel w h) row)

pixelWrapper: String -> String -> String -> Html Msg
pixelWrapper w h pixel =
  span [ style
    [ ("display", "inline-block")
    , ("min-width", w)
    , ("max-width", w)
    , ("min-height", h)
    , ("max-height", h)]]
  [ text pixel ]

viewPixel w h pixel =
  pixelWrapper w h (
    if pixel > 499 then
      "#"
    else if pixel > 13 then
      "X"
    else if pixel > 2 then
      "+"
    else if pixel > 1 then
      "~"
    else if pixel > 0 then
      "`"
    else if pixel == -1 then
      "W"
    else if pixel == -2 then
      "S"
    else if pixel == -3 then
      "A"
    else if pixel == -4 then
      "D"
    else if pixel == -5 then
      "Q"
    else if pixel == -6 then
      "E"
    else
      "!"
  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg,
          Window.resizes DimsMsg
        ]
