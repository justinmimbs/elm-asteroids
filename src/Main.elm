module Main exposing (main, view)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)


-- project modules

import Screen


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = \{ player } -> view [ player ]
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
    { player : Entity
    , controls : Controls
    }


type alias Entity =
    Moving (Renderable {})


type alias Renderable a =
    { a
        | polylines : List Polyline
        , scale : Float
        , position : Vec3
        , rotation : Radians
    }


type alias Moving a =
    { a
        | velocity : Vec3
        , rotationInertia : Radians
    }


type alias Polyline =
    List Vec3


type alias Radians =
    Float


type alias Controls =
    { left : Bool
    , right : Bool
    , thrust : Bool
    }


initialModel : Model
initialModel =
    { player =
        { polylines = spaceship
        , scale = 20
        , position = vec3Zero
        , rotation = 0
        , velocity = vec3Zero
        , rotationInertia = 0
        }
    , controls =
        { left = False
        , right = False
        , thrust = False
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

        _ ->
            NoOp


update : Msg -> Model -> Model
update msg ({ player, controls } as model) =
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
                    )
            }

        Tick ->
            { model
                | player = updatePlayer controls player
            }


settings =
    { thrustRadians = 0.03
    , thrustDistance = 0.8
    , positionFriction = 0.98
    , rotationFriction = 0.8
    }


updatePlayer : Controls -> Entity -> Entity
updatePlayer controls entity =
    let
        { thrustRadians, thrustDistance, positionFriction, rotationFriction } =
            settings

        rotationThrust =
            case ( controls.left, controls.right ) of
                ( True, False ) ->
                    thrustRadians

                ( False, True ) ->
                    thrustRadians |> negate

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
        |> List.map (List.map toVec3)


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


globalTransform : Mat4
globalTransform =
    Matrix4.identity
        |> Matrix4.translate3
            (Tuple.first screenSize / 2)
            (Tuple.second screenSize / 2)
            0
        |> Matrix4.scale3 1 -1 1


view : List (Renderable a) -> Html b
view objects =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            ]
        ]
        [ objects
            |> List.concatMap (transformRenderable globalTransform)
            |> Screen.render screenSize
        ]


transformRenderable : Mat4 -> Renderable a -> List Polyline
transformRenderable parentTransform object =
    let
        transform =
            Matrix4.identity
                |> Matrix4.translate object.position
                |> Matrix4.rotate object.rotation Vector3.k
                |> Matrix4.scale3 object.scale object.scale object.scale
                |> Matrix4.mul parentTransform
    in
        List.map
            (List.map (Matrix4.transform transform))
            object.polylines


toVec3 : ( Float, Float ) -> Vec3
toVec3 ( x, y ) =
    Vector3.vec3 x y 0


vec3Zero : Vec3
vec3Zero =
    Vector3.vec3 0 0 0
