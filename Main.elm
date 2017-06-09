import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Keyboard exposing (..)
import Time exposing (..)
import Css exposing (padding, px)
import Char exposing (fromCode)
import Debug

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


segmentDim = 30
(width, height) = (600, 600)

-- MODEL

type alias Position = (Int, Int)

type Direction
  = Up
  | Down
  | Left
  | Right

type alias Snake =
  { head: Position
  , tail: List Position
  , direction: Direction }

type alias Model = Snake

init : ( Model, Cmd Msg )
init =
  ( Snake (0, 0) [] Right, Cmd.none )


-- UPDATE

--type alias Msg = Direction
type Msg = KeyMsg Keyboard.KeyCode
         | Tick Time

tickModel : Model -> Model
tickModel model =
  case model.direction of
    Up -> {model | head = (Tuple.first model.head, (Tuple.second model.head + 1) % 20)}
    Down -> {model | head = (Tuple.first model.head, (Tuple.second model.head - 1) % 20)}
    Left -> {model | head = ((Tuple.first model.head - 1) % 20, Tuple.second model.head)}
    Right -> {model | head = ((Tuple.first model.head + 1) % 20, Tuple.second model.head )}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyMsg code ->
      -- let l = Debug.log "Key " (fromCode code)
      -- in (model, Cmd.none)
      case fromCode code of
        'W' -> ({model | direction = Up}, Cmd.none)
        'A' -> ({model | direction = Left}, Cmd.none)
        'S' -> ({model | direction = Down}, Cmd.none)
        'D' -> ({model | direction = Right}, Cmd.none)
        _  -> (model, Cmd.none)
    Tick newTime ->
      (tickModel model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Keyboard.downs KeyMsg
            , Time.every second Tick]


-- VIEW

styles =
    Css.asPairs >> Html.Attributes.style

gridToPx : Snake -> (Float, Float)
gridToPx s =
  let px1  = toFloat (Tuple.first s.head * segmentDim) - (width/2)
      px2  = toFloat (Tuple.second s.head * segmentDim) - (height/2)
  in ( px1 + (segmentDim / 2), px2 + (segmentDim/2))

snake : Snake -> Element
snake s = (collage width height
            [ fittedImage width height "http://localhost:8000/wheat.jpg" |> toForm
            , image (round (segmentDim * 1.5)) (round (segmentDim * 1.5)) "http://localhost:8000/scary.png" |> toForm |> move (gridToPx s)
            , segment (-width/2, -height/2) (-width/2, height/2) |> traced (solid black)
            , segment (-width/2, -height/2) (width/2, -height/2) |> traced (solid black)
            , segment (width/2, -height/2) (width/2, height/2) |> traced (solid black)
            , segment (-width/2, height/2) (width/2, height/2) |> traced (solid black)
            ]
          )


view : Model -> Html Msg
view model =
  div
    [ styles [ padding (px 10) ] ]
    [ (snake model) |> Element.toHtml ]
