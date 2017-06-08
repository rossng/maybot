import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Css exposing (padding, px)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


segmentDim = 15
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

type alias Msg = Direction

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

styles =
    Css.asPairs >> Html.Attributes.style

snake : Snake -> Element
snake s = (collage 300 300
            [ square 10 |> outlined (solid black)
            , segment (-150, -150) (-150, 150) |> traced (solid black)
            , segment (-150, -150) (150, -150) |> traced (solid black)
            , segment (150, -150) (150, 150) |> traced (solid black)
            , segment (-150, 150) (150, 150) |> traced (solid black)
            ]
          )


view : Model -> Html Msg
view model =
  div
    [ styles [ padding (px 10) ] ]
    [ (snake model) |> Element.toHtml ]
