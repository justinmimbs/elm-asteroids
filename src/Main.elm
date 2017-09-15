module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Random.Pcg as Random
import Time exposing (Time)


-- project modules

import Level exposing (Controls, Level)
import Screen


main : Program Never Model Msg
main =
    Html.program
        { init = ( init (Random.initialSeed 3780540833), Cmd.none )
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
    { seed = seed
    , controls = Level.initialControls
    , level = Level.init screenSize seed
    }


type alias Model =
    { seed : Random.Seed
    , controls : Controls
    , level : Level
    }


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )



-- update


type Msg
    = NoOp
    | Input Control Bool
    | Tick Time


type Control
    = Left
    | Right
    | Thrust
    | Fire
    | Shield


keyCodeToMsg : Bool -> Int -> Msg
keyCodeToMsg state keyCode =
    case keyCode of
        -- left
        37 ->
            Input Left state

        -- up
        38 ->
            Input Thrust state

        -- right
        39 ->
            Input Right state

        -- f
        70 ->
            Input Fire state

        -- s
        83 ->
            Input Shield state

        _ ->
            NoOp


update : Msg -> Model -> Model
update msg ({ controls } as model) =
    case msg of
        NoOp ->
            model

        Input control state ->
            { model
                | controls =
                    (case control of
                        Left ->
                            { controls | left = state }

                        Right ->
                            { controls | right = state }

                        Thrust ->
                            { controls | thrust = state }

                        Fire ->
                            { controls | fire = state }

                        Shield ->
                            { controls | shield = state }
                    )
            }

        Tick dt ->
            { model
                | level = model.level |> Level.update dt controls
            }



-- view


view : Model -> Html a
view =
    .level >> Level.toPaths >> viewPaths


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
