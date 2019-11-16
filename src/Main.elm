module Main exposing (main)

import Asteroid exposing (Asteroid)
import Browser
import Browser.Events
import Font exposing (Font)
import Font.Astraea as Astraea
import Geometry.Vector as Vector exposing (Point)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Level exposing (Controls, Level)
import PathData exposing (PathData)
import Random
import Screen
import Static
import Types exposing (Moving, Polyline, Positioned)
import Util exposing (transformPoints, wrapPosition)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Random.initialSeed 3780540839 |> init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onKeyDown (decodeKeyEvent True)
                    , Browser.Events.onKeyUp (decodeKeyEvent False)
                    , Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))
                    ]
        }


init : Random.Seed -> Model
init seed =
    ( seed, initMainTitle seed )


type alias Model =
    ( Random.Seed, State )


type State
    = MainTitle (List Asteroid)
    | LevelTitle Int Level Time
    | Playing Int Level Controls
    | Cleared Int Level Controls Time
    | Destroyed Int Level Time


type alias Time =
    Float


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


initMainTitle : Random.Seed -> State
initMainTitle =
    Random.step (Asteroid.field screenSize 0 10)
        >> Tuple.first
        >> MainTitle


initLevel : Int -> Random.Seed -> State
initLevel n seed =
    LevelTitle n (Level.init screenSize n (levelSeed n seed) |> Level.asteroidsUpdate -1.5) 1.5


levelSeed : Int -> Random.Seed -> Random.Seed
levelSeed n =
    fastForward (n + 1)
        >> Random.step Random.independentSeed
        >> Tuple.first


fastForward : Int -> Random.Seed -> Random.Seed
fastForward steps seed =
    if steps <= 0 then
        seed

    else
        fastForward (steps - 1) (Random.step (Random.constant ()) seed |> Tuple.second)



-- update


type Msg
    = NoOp
    | Input Control Bool
    | Start
    | Tick Time


type Control
    = Left
    | Right
    | Thrust
    | Fire
    | Shield


decodeKeyEvent : Bool -> Decoder Msg
decodeKeyEvent isPressed =
    Decode.field "key" Decode.string
        |> Decode.andThen (keyToMsg isPressed >> decoderFromMaybe)


decoderFromMaybe : Maybe a -> Decoder a
decoderFromMaybe maybe =
    case maybe of
        Just x ->
            Decode.succeed x

        Nothing ->
            Decode.fail "Nothing"


keyToMsg : Bool -> String -> Maybe Msg
keyToMsg isPressed key =
    case key of
        "Enter" ->
            if isPressed then
                Just Start

            else
                Nothing

        "ArrowLeft" ->
            Just <| Input Left isPressed

        "ArrowUp" ->
            Just <| Input Thrust isPressed

        "ArrowRight" ->
            Just <| Input Right isPressed

        "f" ->
            Just <| Input Fire isPressed

        "F" ->
            Just <| Input Fire isPressed

        "s" ->
            Just <| Input Shield isPressed

        "S" ->
            Just <| Input Shield isPressed

        _ ->
            Nothing


update : Msg -> Model -> Model
update msg (( seed, state ) as model) =
    (case msg of
        NoOp ->
            state

        Start ->
            case state of
                MainTitle _ ->
                    initLevel 1 seed

                Destroyed n _ _ ->
                    initLevel n seed

                _ ->
                    state

        Input control isPressed ->
            case state of
                Playing n level controls ->
                    Playing n level (controls |> updateControls control isPressed)

                Cleared n level controls timer ->
                    Cleared n level (controls |> updateControls control isPressed) timer

                _ ->
                    state

        Tick dt ->
            case state of
                MainTitle asteroids ->
                    MainTitle (asteroids |> List.map (updateMoving dt >> wrapPosition screenSize))

                LevelTitle n level timer ->
                    if timer > dt then
                        LevelTitle n (level |> Level.asteroidsUpdate dt) (timer - dt)

                    else
                        Playing n level Level.initialControls

                Playing n level controls ->
                    case level |> Level.update dt controls of
                        ( levelUpdated, Nothing ) ->
                            Playing n levelUpdated controls

                        ( levelUpdated, Just Level.Cleared ) ->
                            Cleared n levelUpdated controls 3.0

                        ( levelUpdated, Just Level.Destroyed ) ->
                            Destroyed n levelUpdated 7.0

                Cleared n level controls timer ->
                    if timer > dt then
                        Cleared n (level |> Level.update dt controls |> Tuple.first) controls (timer - dt)

                    else
                        initLevel (n + 1) seed

                Destroyed n level timer ->
                    if timer > dt then
                        Destroyed n (level |> Level.update dt Level.initialControls |> Tuple.first) (timer - dt)

                    else
                        initMainTitle seed
    )
        |> Tuple.pair seed


updateControls : Control -> Bool -> Controls -> Controls
updateControls control isPressed controls =
    case control of
        Left ->
            { controls | left = isPressed }

        Right ->
            { controls | right = isPressed }

        Thrust ->
            { controls | thrust = isPressed }

        Fire ->
            { controls | fire = isPressed }

        Shield ->
            { controls | shield = isPressed }


updateMoving : Time -> Moving (Positioned a) -> Moving (Positioned a)
updateMoving dt obj =
    { obj
        | position = obj.position |> Vector.add (obj.velocity |> Vector.scale dt)
        , rotation = obj.rotation + obj.angularVelocity * dt
    }



-- view


view : Model -> Browser.Document a
view ( _, state ) =
    case state of
        MainTitle asteroids ->
            ("ASTEROIDS" |> typeset largeFont screenCenter)
                ++ ("PRESS ENTER" |> typeset smallFont (screenCenter |> Vector.add ( 0, 3 * smallFont.height )))
                ++ (asteroids |> List.map asteroidToPath)
                |> viewMain

        LevelTitle n level _ ->
            ("LEVEL " ++ String.fromInt n |> typeset mediumFont (screenCenter |> Vector.add ( 0, 0.5 * mediumFont.height )))
                ++ (level |> Level.asteroidsToPaths)
                |> viewMain

        Playing _ level _ ->
            level |> Level.toPaths |> viewMain

        Cleared _ level _ timer ->
            if timer > 2 then
                level |> Level.toPaths |> viewMain

            else
                ("CLEARED" |> typeset mediumFont (screenCenter |> Vector.add ( 0, 0.5 * mediumFont.height )))
                    ++ (level |> Level.toPaths)
                    |> viewMain

        Destroyed _ level timer ->
            if timer > 5 then
                level |> Level.toPaths |> viewMain

            else
                ("PRESS ENTER TO CONTINUE" |> typeset smallFont (screenCenter |> Vector.add ( 0, -2 * smallFont.height )))
                    ++ (timer |> ceiling |> String.fromInt |> typeset mediumFont (screenCenter |> Vector.add ( 0, 1 * mediumFont.height )))
                    ++ (level |> Level.toPaths)
                    |> viewMain


viewMain : List Screen.Path -> Browser.Document a
viewMain paths =
    Browser.Document
        "Asteroids"
        [ Html.node "style"
            []
            [ Html.text "@import url(../app/style.css);"
            ]
        , Html.main_
            []
            [ Html.div
                [ Html.Attributes.class "screen-container"
                ]
                [ paths |> render
                ]
            , Html.div
                [ Html.Attributes.class "instructions-container"
                ]
                [ Static.instructions
                ]
            ]
        ]


render : List Screen.Path -> Html a
render =
    Screen.render screenSize


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    Screen.Path 0.5 True (polygon |> transformPoints position rotation)


screenCenter : Point
screenCenter =
    screenSize |> Vector.scale 0.5



-- typography


largeFont : Font (List Polyline)
largeFont =
    Astraea.pathData
        |> Font.scale PathData.scale 3
        |> Font.map (PathData.toPolylines 26)


mediumFont : Font (List Polyline)
mediumFont =
    Astraea.pathData
        |> Font.scale PathData.scale 2
        |> Font.map (PathData.toPolylines 18)


smallFont : Font (List Polyline)
smallFont =
    Astraea.pathData
        |> Font.scale PathData.scale (2 / 3)
        |> Font.map (PathData.toPolylines 10)
        |> (\a -> { a | width = a.width + 2 })


typeset : Font (List Polyline) -> Point -> String -> List Screen.Path
typeset font ( cx, cy ) text =
    let
        ( ox, oy ) =
            ( cx - ((toFloat (String.length text) * font.width) / 2)
            , cy - font.height
            )
    in
    Font.typesetLine (\x -> List.map (List.map (Vector.add ( x, oy )))) font ox text
        |> List.concatMap (List.map (Screen.Path 1 False))
