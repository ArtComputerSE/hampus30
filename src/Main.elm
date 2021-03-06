module Main exposing (main)

import Browser
import Html exposing (Html, button, div)
import Html.Attributes
import Html.Events exposing (onClick)
import Keyboard exposing (Key(..))
import Random
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


speed =
    200


type alias Model =
    { now : Time.Posix
    , age : Int
    , gameState : GameState
    , snake : Snake
    , apple : Coord
    , pressedKeys : List Key
    }


type GameState
    = RUN
    | EAT
    | WON
    | END


type Direction
    = NORTH
    | SOUTH
    | WEST
    | EAST


type alias Coord =
    { x : Int
    , y : Int
    }


type alias Snake =
    { direction : Direction
    , head : Coord
    , tail : List Coord
    }


type Command
    = LEFT
    | UP
    | RIGHT
    | DOWN
    | START
    | NONE


initialSnake : Snake
initialSnake =
    { direction = WEST
    , head =
        { x = 48
        , y = 48
        }
    , tail =
        [ { x = 52
          , y = 48
          }
        ]
    }


initialApple : Coord
initialApple =
    { x = 12
    , y = 36
    }


initialModel : Model
initialModel =
    { now = Time.millisToPosix 0
    , age = 0
    , gameState = RUN
    , snake = initialSnake
    , apple = initialApple
    , pressedKeys = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = Tick Time.Posix
    | KeyMsg Keyboard.Msg
    | Button Command
    | EatApple
    | NewApple ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            updateGame model newTime

        KeyMsg keyMsg ->
            updateOnKeyPress { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }

        Button command ->
            updateDirection model command

        EatApple ->
            eatApple model

        NewApple ( x, y ) ->
            newApple model (Coord (x * 4) (y * 4))


updateGame : Model -> Time.Posix -> ( Model, Cmd Msg )
updateGame model newTime =
    ( case model.gameState of
        RUN ->
            { model | now = newTime, snake = updateSnake model, gameState = updateGameState model }

        EAT ->
            model

        WON ->
            { model | now = newTime }

        END ->
            { model | now = newTime }
    , nextCmd model
    )


nextCmd : Model -> Cmd Msg
nextCmd model =
    if model.apple == model.snake.head then
        send EatApple

    else
        Cmd.none


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


updateSnake : Model -> Snake
updateSnake model =
    case model.snake.direction of
        NORTH ->
            moveSnake 0 -4 model.snake model.age

        SOUTH ->
            moveSnake 0 4 model.snake model.age

        WEST ->
            moveSnake -4 0 model.snake model.age

        EAST ->
            moveSnake 4 0 model.snake model.age


moveSnake : Int -> Int -> Snake -> Int -> Snake
moveSnake updateX updateY snake age =
    { snake | head = moveHead snake.head updateX updateY, tail = moveTail snake.head snake.tail age }


moveHead : Coord -> Int -> Int -> Coord
moveHead head updateX updateY =
    { y = head.y + updateY, x = head.x + updateX }


moveTail : Coord -> List Coord -> Int -> List Coord
moveTail head tail age =
    head :: List.take (tailLengthFromAge age) tail


tailLengthFromAge : Int -> Int
tailLengthFromAge age =
    if age < 5 then
        age

    else if age < 10 then
        age + 5

    else if age < 15 then
        age + 10

    else if age < 20 then
        age + 15

    else if age < 25 then
        age + 20

    else
        age + 25


updateGameState : Model -> GameState
updateGameState model =
    if withinBounds model && not (hittingSelf model) then
        RUN

    else
        END


withinBounds : Model -> Bool
withinBounds model =
    let
        head =
            model.snake.head
    in
    (head.y >= 4) && (head.y <= 96) && (head.x >= 4) && (head.x <= 96)


hittingSelf : Model -> Bool
hittingSelf model =
    List.any (\m -> m == model.snake.head) model.snake.tail


eatApple : Model -> ( Model, Cmd Msg )
eatApple model =
    if model.age >= 29 then
        ( { model | gameState = WON, age = 30 }, Cmd.none )

    else
        ( { model | gameState = EAT, age = model.age + 1 }
        , Random.generate NewApple randomPoint
        )


newApple : Model -> Coord -> ( Model, Cmd Msg )
newApple model coord =
    if (model.snake.head == coord) || List.member coord model.snake.tail then
        ( model, Random.generate NewApple randomPoint )

    else
        ( { model | apple = { x = coord.x, y = coord.y }, gameState = RUN }, Cmd.none )


randomPoint : Random.Generator ( Int, Int )
randomPoint =
    Random.pair (Random.int 1 24) (Random.int 1 24)


updateDirection : Model -> Command -> ( Model, Cmd Msg )
updateDirection model command =
    case command of
        LEFT ->
            newDirection model WEST

        UP ->
            newDirection model NORTH

        RIGHT ->
            newDirection model EAST

        DOWN ->
            newDirection model SOUTH

        START ->
            ( if model.gameState == END then
                initialModel

              else
                model
            , Cmd.none
            )

        NONE ->
            ( model, Cmd.none )


updateOnKeyPress : Model -> ( Model, Cmd Msg )
updateOnKeyPress model =
    let
        key =
            List.head model.pressedKeys |> Maybe.withDefault Keyboard.Backspace
    in
    case key of
        ArrowLeft ->
            newDirection model WEST

        ArrowUp ->
            newDirection model NORTH

        ArrowRight ->
            newDirection model EAST

        ArrowDown ->
            newDirection model SOUTH

        Spacebar ->
            ( if model.gameState == END then
                initialModel

              else
                model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


newDirection : Model -> Direction -> ( Model, Cmd Msg )
newDirection model dir =
    ( { model | snake = newSnakeDirection model.snake dir }, Cmd.none )


newSnakeDirection : Snake -> Direction -> Snake
newSnakeDirection snake dir =
    { snake | direction = dir }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every speed Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style "padding" "10px"
        , Html.Attributes.style "border" "solid 1px"
        ]
        [ svg [ viewBox "0 0 100 100", width "50%" ]
            (gameView model)
        , div [ class "commandBox" ]
            [ div [ class "commandRow" ]
                [ button [ onClick (Button START), class "commandButton" ] [ text "SPACE" ] ]
            , div [ class "commandRow" ]
                [ button [ onClick (Button UP), class "commandButton" ] [ text "UP" ] ]
            , div [ class "commandRow" ]
                [ button [ onClick (Button LEFT), class "commandButton" ] [ text "LEFT" ]
                , button [ onClick (Button RIGHT), class "commandButton" ] [ text "RIGHT" ]
                ]
            , div [ class "commandRow" ]
                [ button [ onClick (Button DOWN), class "commandButton" ] [ text "DOWN" ] ]
            ]
        , div []
            [ text (clockView model)
            , text " Age: "
            , text (String.fromInt model.age)
            ]
        ]


gameView : Model -> List (Svg Msg)
gameView model =
    background model
        ++ appleView model.apple
        ++ snakeView model.snake


clockView : Model -> String
clockView model =
    let
        z =
            Time.utc
    in
    String.fromInt (Time.toHour z model.now)
        ++ ":"
        ++ String.fromInt (Time.toMinute z model.now)


background : Model -> List (Svg Msg)
background model =
    [ rect [ width "100", height "100", fill "lightBlue" ]
        []
    , text_ [ x "10", y "25", fontFamily "Verdana", fontSize "7", fill "black" ]
        [ text
            (messageBasedOnState model)
        ]
    , text_ [ x "30", y "75", fontFamily "Verdana", fontSize "7", fill "black" ]
        [ text <| "Age: " ++ String.fromInt model.age
        ]
    ]


messageBasedOnState : Model -> String
messageBasedOnState model =
    case model.gameState of
        RUN ->
            "Hampus must be 30!"

        EAT ->
            "Eating"

        WON ->
            "Congratulations!"

        END ->
            "Press space to restart!"


snakeView : Snake -> List (Svg Msg)
snakeView snake =
    tailView snake ++ headView snake


headView : Snake -> List (Svg Msg)
headView snake =
    let
        size =
            12

        y =
            snake.head.y - size // 2

        x =
            snake.head.x - size // 2

        translate =
            "translate(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"
    in
    [ image
        [ xlinkHref (imageBasedOnDirection snake.direction)
        , width (String.fromInt size)
        , height (String.fromInt size)
        , transform translate
        ]
        []
    ]


imageBasedOnDirection : Direction -> String
imageBasedOnDirection direction =
    case direction of
        NORTH ->
            "image/hampus-north.png"

        SOUTH ->
            "image/hampus-south.png"

        WEST ->
            "image/hampus-west.png"

        EAST ->
            "image/hampus-east.png"


tailView : Snake -> List (Svg Msg)
tailView snake =
    List.map tailPart snake.tail


tailPart : Coord -> Svg Msg
tailPart coord =
    circle [ cx (String.fromInt coord.x), cy (String.fromInt coord.y), r "2", fill "green" ] []


appleView : Coord -> List (Svg Msg)
appleView coord =
    [ image
        [ xlinkHref "image/apple.png"
        , x (String.fromInt (coord.x - 6))
        , y (String.fromInt (coord.y - 6))
        , width "12"
        , height "12"
        ]
        []
    ]
