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
        { init = ( Random.initialSeed 3780540839 |> init, Cmd.none )
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
    | LevelTitle Int Level Time
    | Playing Int Level Controls
    | Cleared Int Level Controls Time
    | Destroyed Int Level Time


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
    LevelTitle n (Level.init screenSize n (levelSeed n seed)) 1.0


levelSeed : Int -> Random.Seed -> Random.Seed
levelSeed n =
    Random.fastForward (n + 1)
        >> Random.step Random.independentSeed
        >> Tuple.first



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
                        LevelTitle n (level |> Level.update dt Level.initialControls |> Tuple.first) (timer - dt)
                    else
                        Playing n level Level.initialControls

                Playing n level controls ->
                    case level |> Level.update dt controls of
                        ( levelUpdated, Nothing ) ->
                            Playing n levelUpdated controls

                        ( levelUpdated, Just Level.Cleared ) ->
                            Cleared n levelUpdated controls 4.0

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
            ("ASTEROIDS" |> typeset largeFont screenCenter)
                ++ ("PRESS ENTER" |> typeset smallFont (screenCenter |> Vector.add ( 0, 3 * smallFont.height )))
                ++ (asteroids |> List.map asteroidToPath)
                |> viewPaths

        LevelTitle n level _ ->
            ("LEVEL " ++ toString n |> typeset mediumFont (screenCenter |> Vector.add ( 0, 0.5 * mediumFont.height )))
                ++ ({ level | player = Nothing } |> Level.toPaths)
                |> viewPaths

        Playing _ level _ ->
            level |> Level.toPaths |> viewPaths

        Cleared _ level _ timer ->
            if timer > 3 then
                level |> Level.toPaths |> viewPaths
            else
                ("CLEARED" |> typeset mediumFont (screenCenter |> Vector.add ( 0, 0.5 * mediumFont.height )))
                    ++ (level |> Level.toPaths)
                    |> viewPaths

        Destroyed _ level timer ->
            if timer > 5 then
                level |> Level.toPaths |> viewPaths
            else
                ("PRESS ENTER TO CONTINUE" |> typeset smallFont (screenCenter |> Vector.add ( 0, -2 * smallFont.height )))
                    ++ (timer |> ceiling |> toString |> typeset mediumFont (screenCenter |> Vector.add ( 0, 1 * mediumFont.height )))
                    ++ (level |> Level.toPaths)
                    |> viewPaths


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


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    ( 0.5, True, polygon |> transformPoints position rotation )


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
            |> List.concatMap (List.map ((,,) 1 False))
