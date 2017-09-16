module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Random.Pcg as Random
import Time exposing (Time)


-- project modules

import Asteroid exposing (Asteroid)
import Font exposing (Font)
import Font.Astraea as Astraea
import Geometry.Vector as Vector exposing (Point)
import Level exposing (Controls, Level)
import PathData exposing (PathData)
import Screen
import Types exposing (Moving, Positioned, Polyline)
import Util exposing (transformPoints, wrapPosition)


main : Program Never Model Msg
main =
    Html.program
        { init = ( Random.initialSeed 3780540833 |> init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions =
            always
                (Sub.batch
                    [ Keyboard.downs (keyCodeToMsg True)
                    , Keyboard.ups (keyCodeToMsg False)
                    , AnimationFrame.diffs (Tick << Time.inSeconds)
                    ]
                )
        }


init : Random.Seed -> Model
init seed =
    ( seed, initMainTitle seed )


type alias Model =
    ( Random.Seed, State )


type State
    = MainTitle (List Asteroid)
    | Playing Int Level Controls


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
    let
        levelSeed =
            seed |> Random.fastForward n |> Random.step Random.independentSeed |> Tuple.first
    in
        Playing n (Level.init screenSize levelSeed) Level.initialControls



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


keyCodeToMsg : Bool -> Int -> Msg
keyCodeToMsg isPressed keyCode =
    case keyCode of
        -- enter
        13 ->
            if isPressed then
                Start
            else
                NoOp

        -- left
        37 ->
            Input Left isPressed

        -- up
        38 ->
            Input Thrust isPressed

        -- right
        39 ->
            Input Right isPressed

        -- f
        70 ->
            Input Fire isPressed

        -- s
        83 ->
            Input Shield isPressed

        _ ->
            NoOp


update : Msg -> Model -> Model
update msg (( seed, state ) as model) =
    (case msg of
        NoOp ->
            state

        Start ->
            case state of
                MainTitle _ ->
                    initLevel 1 seed

                _ ->
                    state

        Input control isPressed ->
            case state of
                Playing n level controls ->
                    Playing n level (controls |> updateControls control isPressed)

                _ ->
                    state

        Tick dt ->
            case state of
                MainTitle asteroids ->
                    MainTitle (asteroids |> List.map (updateMoving dt >> wrapPosition screenSize))

                Playing n level controls ->
                    Playing n (level |> Level.update dt controls) controls
    )
        |> (,) seed


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


view : Model -> Html a
view ( _, state ) =
    case state of
        MainTitle asteroids ->
            let
                center =
                    screenSize |> Vector.scale 0.5
            in
                typesetForScreen titleFont center "ASTEROIDS"
                    ++ typesetForScreen bodyFont (center |> Vector.add ( 0, 3 * bodyFont.height )) "PRESS ENTER"
                    ++ (asteroids |> List.map asteroidToPath)
                    |> viewPaths

        Playing _ level _ ->
            level |> Level.toPaths |> viewPaths


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    ( 0.5, True, polygon |> transformPoints position rotation )


viewPaths : List Screen.Path -> Html a
viewPaths paths =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            ]
        ]
        [ paths |> Screen.render screenSize
        ]



-- typography


titleFont : Font (List Polyline)
titleFont =
    Astraea.pathData
        |> Font.scale PathData.scale 3
        |> Font.map (PathData.toPolylines 26)


bodyFont : Font (List Polyline)
bodyFont =
    Astraea.pathData
        |> Font.map (PathData.toPolylines 13)
        |> (\a -> { a | width = a.width + 2 })


typesetForScreen : Font (List Polyline) -> Point -> String -> List Screen.Path
typesetForScreen font ( cx, cy ) text =
    let
        ( ox, oy ) =
            ( cx - ((toFloat (String.length text) * font.width) / 2)
            , cy - font.height
            )
    in
        Font.typesetLine (\x -> List.map (List.map (Vector.add ( x, oy )))) font ox text
            |> List.concatMap (List.map ((,,) 1 False))
