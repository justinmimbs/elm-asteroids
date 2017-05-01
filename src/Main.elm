module Main exposing (main, view, wrapPosition)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import Time exposing (Time)


-- project modules

import Screen
import Types exposing (Radians, Polyline, Renderable, Moving)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = \{ player, blasts } -> view (renderable player :: List.map renderable blasts)
        , subscriptions =
            always
                (Sub.batch
                    [ Keyboard.downs (keyCodeToMsg True)
                    , Keyboard.ups (keyCodeToMsg False)
                    , AnimationFrame.diffs (Tick << Time.inSeconds)
                    ]
                )
        }



-- model


type alias Model =
    { player : Player
    , blaster : Blaster
    , blasts : List Blast
    , controls : Controls
    }


type alias Player =
    Moving (Renderable {})


{-| Represents time until next fire.
-}
type alias Blaster =
    Maybe Time


type alias Blast =
    Moving (Renderable { timeRemaining : Time })


type alias Controls =
    { left : Bool
    , right : Bool
    , thrust : Bool
    , fire : Bool
    }


initialModel : Model
initialModel =
    { player =
        { polylines = spaceship
        , position = screenSize |> toVec3 |> Vector3.scale 0.5
        , rotation = pi
        , velocity = vec3Zero
        , rotationInertia = 0
        }
    , blaster = Nothing
    , blasts = []
    , controls =
        { left = False
        , right = False
        , thrust = False
        , fire = False
        }
    }



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
                    )
            }

        Tick dt ->
            let
                playerNext =
                    model.player |> updatePlayer dt controls |> wrapPosition

                blasterNext =
                    model.blaster |> updateBlaster dt controls.fire

                blastsNext =
                    blasterNext
                        |> Maybe.andThen (fireBlast dt playerNext)
                        |> Maybe.map ((flip (::)) model.blasts)
                        |> Maybe.withDefault model.blasts
                        |> List.filterMap (updateBlast dt)
                        |> List.map wrapPosition
            in
                { model
                    | player = playerNext
                    , blaster = blasterNext
                    , blasts = blastsNext
                }


fireBlast : Time -> Player -> Time -> Maybe Blast
fireBlast dt player timeTilFire =
    if timeTilFire < 0.001 then
        let
            -- px / second
            speed =
                Vector3.length player.velocity + 600
        in
            Just
                { polylines = [ [ vec3Zero, Vector3.vec3 0 (speed * dt) 0 ] ]
                , position = player.position
                , rotation = player.rotation
                , velocity = ( speed, player.rotation + pi / 2 ) |> fromPolar |> toVec3
                , rotationInertia = 0
                , timeRemaining = 1200 / speed
                }
    else
        Nothing


updateBlast : Time -> Blast -> Maybe Blast
updateBlast dt blast =
    if blast.timeRemaining > 0 then
        Just
            { blast
                | position = blast.position |> Vector3.add (blast.velocity |> Vector3.scale dt)
                , timeRemaining = blast.timeRemaining - dt
            }
    else
        Nothing


updateBlaster : Time -> Bool -> Blaster -> Blaster
updateBlaster dt fire blaster =
    if fire then
        case blaster of
            Nothing ->
                Just 0

            Just t ->
                if t < 0.001 then
                    -- 6 hz
                    Just (1 / 6 - dt)
                else
                    Just (t - dt)
    else
        Nothing


playerSettings =
    { thrustRadians = 1.8 -- rad / second
    , thrustDistance = 35 -- px / second
    , positionFriction = 0.98
    , rotationFriction = 0.8
    }


updatePlayer : Time -> Controls -> Player -> Player
updatePlayer dt controls entity =
    let
        { thrustRadians, thrustDistance, positionFriction, rotationFriction } =
            playerSettings

        rotationThrust =
            case ( controls.left, controls.right ) of
                ( True, False ) ->
                    thrustRadians * dt |> negate

                ( False, True ) ->
                    thrustRadians * dt

                _ ->
                    0

        rotationNext =
            entity.rotation
                + (entity.rotationInertia * rotationFriction * dt)
                + rotationThrust

        positionThrust =
            if controls.thrust then
                ( thrustDistance * dt, rotationNext + pi / 2 ) |> fromPolar |> toVec3
            else
                vec3Zero

        positionNext =
            entity.position
                |> Vector3.add (entity.velocity |> Vector3.scale (positionFriction * dt))
                |> Vector3.add (positionThrust)
    in
        { entity
            | position = positionNext
            , rotation = rotationNext
            , velocity = Vector3.sub positionNext entity.position |> Vector3.scale (1 / dt)
            , rotationInertia = (rotationNext - entity.rotation) / dt
        }



-- view


spaceship : List Polyline
spaceship =
    [ [ ( 0.5, -1 )
      , ( 0, 1 )
      , ( -0.5, -1 )
      , ( 0.5, -1 )
      ]
    , [ ( 0.5, -1 )
      , ( 1, -0.5 )
      , ( 0, 0 )
      , ( -1, -0.5 )
      , ( -0.5, -1 )
      ]
    ]
        |> List.map (List.map (toVec3 >> Vector3.scale 18))


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


wrapPosition : Renderable a -> Renderable a
wrapPosition object =
    { object
        | position = object.position |> (uncurry wrapVec3) screenSize
    }


wrapVec3 : Float -> Float -> Vec3 -> Vec3
wrapVec3 xMax yMax vec =
    Vector3.vec3
        (floatModulo (Vector3.getX vec) xMax)
        (floatModulo (Vector3.getY vec) yMax)
        0


floatModulo : Float -> Float -> Float
floatModulo x y =
    let
        n =
            x / y |> floor |> toFloat
    in
        x - n * y


view : List (Renderable a) -> Html b
view objects =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            ]
        ]
        [ objects
            |> List.concatMap transformRenderable
            |> Screen.render screenSize
        ]


transformRenderable : Renderable a -> List Polyline
transformRenderable object =
    let
        transform =
            Matrix4.identity
                |> Matrix4.translate object.position
                |> Matrix4.rotate object.rotation Vector3.k
    in
        List.map
            (List.map (Matrix4.transform transform))
            object.polylines


{-| Strip extended fields. Refactor to remove this step!
-}
renderable : Renderable a -> Renderable {}
renderable { polylines, position, rotation } =
    { polylines = polylines
    , position = position
    , rotation = rotation
    }


toVec3 : ( Float, Float ) -> Vec3
toVec3 ( x, y ) =
    Vector3.vec3 x y 0


vec3Zero : Vec3
vec3Zero =
    Vector3.vec3 0 0 0
