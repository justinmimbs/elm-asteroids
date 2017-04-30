module Main exposing (main, view, wrapPosition)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)


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
                    , AnimationFrame.diffs (always Tick)
                    ]
                )
        }



-- model


type alias Model =
    { player : Player
    , blaster : Maybe Int -- timeTilFire
    , blasts : List Blast
    , controls : Controls
    }


type alias Player =
    Moving (Renderable {})


type alias Blast =
    Moving (Renderable { timeRemaining : Int })


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
    | Tick


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

        Tick ->
            let
                playerNext =
                    model.player |> updatePlayer controls |> wrapPosition

                blasterNext =
                    model.blaster |> updateBlaster controls.fire

                blastsNext =
                    fireBlast playerNext blasterNext
                        |> Maybe.map ((flip (::)) model.blasts)
                        |> Maybe.withDefault model.blasts
                        |> List.filterMap updateBlast
                        |> List.map wrapPosition
            in
                { model
                    | player = playerNext
                    , blaster = blasterNext
                    , blasts = blastsNext
                }


fireBlast : Player -> Maybe Int -> Maybe Blast
fireBlast player blaster =
    if blaster == Just 0 then
        let
            length =
                Vector3.length player.velocity + 10
        in
            Just
                { polylines = [ [ vec3Zero, Vector3.vec3 0 length 0 ] ]
                , position = player.position
                , rotation = player.rotation
                , velocity = ( length, player.rotation + pi / 2 ) |> fromPolar |> toVec3
                , rotationInertia = 0
                , timeRemaining = 1200 // (floor length)
                }
    else
        Nothing


updateBlast : Blast -> Maybe Blast
updateBlast blast =
    if blast.timeRemaining > 0 then
        Just
            { blast
                | position = Vector3.add blast.position blast.velocity
                , timeRemaining = blast.timeRemaining - 1
            }
    else
        Nothing


updateBlaster : Bool -> Maybe Int -> Maybe Int
updateBlaster fire blaster =
    if fire then
        case blaster of
            Nothing ->
                Just 0

            Just 0 ->
                Just 10

            Just n ->
                Just (n - 1)
    else
        Nothing


playerSettings =
    { thrustRadians = 0.03
    , thrustDistance = 0.55
    , positionFriction = 0.98
    , rotationFriction = 0.8
    }


updatePlayer : Controls -> Player -> Player
updatePlayer controls entity =
    let
        { thrustRadians, thrustDistance, positionFriction, rotationFriction } =
            playerSettings

        rotationThrust =
            case ( controls.left, controls.right ) of
                ( True, False ) ->
                    thrustRadians |> negate

                ( False, True ) ->
                    thrustRadians

                _ ->
                    0

        rotationNext =
            entity.rotation
                + (entity.rotationInertia * rotationFriction)
                + rotationThrust

        positionThrust =
            if controls.thrust then
                ( thrustDistance, rotationNext + pi / 2 ) |> fromPolar |> toVec3
            else
                vec3Zero

        positionNext =
            entity.position
                |> Vector3.add (entity.velocity |> Vector3.scale positionFriction)
                |> Vector3.add (positionThrust)
    in
        { entity
            | position = positionNext
            , rotation = rotationNext
            , velocity = Vector3.sub positionNext entity.position
            , rotationInertia = rotationNext - entity.rotation
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
