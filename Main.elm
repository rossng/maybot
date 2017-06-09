import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import Time exposing (..)
import Css exposing (padding, px)
import Char exposing (fromCode)
import Text
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

type alias Game =
  { snake: Snake
  , orphan: Maybe Position
  , time: Int
  , score: Int }

type alias Snake =
  { head: Position
  , tail: List Position
  , direction: Direction }

type alias Model = Game

init : ( Model, Cmd Msg )
init =
  ( { snake = Snake (0, 0) [] Right
    , orphan = Nothing
    , time = 0
    , score = 0 }
  , Cmd.none )


-- UPDATE

--type alias Msg = Direction
type Msg = KeyMsg Keyboard.KeyCode
         | Tick Time

tickModel : Model -> Model
tickModel model =
  let oldSnake = model.snake
      newSnake = {oldSnake | head =
        case oldSnake.direction of
          Up -> (Tuple.first oldSnake.head, (Tuple.second oldSnake.head + 1) % 20)
          Down -> (Tuple.first oldSnake.head, (Tuple.second oldSnake.head - 1) % 20)
          Left -> ((Tuple.first oldSnake.head - 1) % 20, Tuple.second oldSnake.head)
          Right -> ((Tuple.first oldSnake.head + 1) % 20, Tuple.second oldSnake.head )}
      newOrphan = case model.orphan of
        Nothing ->
          Just (Tuple.first (Random.step (Random.int 0 19) (initialSeed model.time) ), Tuple.first (Random.step  (Random.int 0 19) (initialSeed (model.time*2))))
        Just orphan -> if model.snake.head == orphan then Nothing else Just orphan
  in { model | snake = newSnake, orphan = newOrphan, time = model.time + 1 }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyMsg code ->
      -- let l = Debug.log "Key " (fromCode code)
      -- in (model, Cmd.none)
      let oldSnake = model.snake
          newSnake = { oldSnake | direction = case fromCode code of
            'W' -> Up
            'A' -> Left
            'S' -> Down
            'D' -> Right
            _ -> oldSnake.direction }
      in ({ model | snake = newSnake }, Cmd.none)
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

gridToPx : Position -> (Float, Float)
gridToPx pos =
  let px1  = toFloat (Tuple.first pos * segmentDim) - (width/2)
      px2  = toFloat (Tuple.second pos * segmentDim) - (height/2)
  in ( px1 + (segmentDim / 2), px2 + (segmentDim/2))

renderGame : Game -> Element
renderGame game = (collage width height
            [ fittedImage width height "http://localhost:8000/wheat.jpg" |> toForm
            , case game.orphan of
                Just orphan -> image (round (segmentDim * 1.2)) (round (segmentDim * 1.2)) "http://localhost:8000/orphan.png" |> toForm |> move (gridToPx orphan)
                Nothing     -> square 0 |> filled black
            , image (round (segmentDim * 1.5)) (round (segmentDim * 1.5)) "http://localhost:8000/scary.png" |> toForm |> move (gridToPx game.snake.head)
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
    [ h1 [ styles [ Css.fontFamilies ["Impact"] ] ] [Html.text "Naughty Theresa"]
    , p [] [Html.text (toString model.score)]
    , (renderGame model) |> Element.toHtml ]
