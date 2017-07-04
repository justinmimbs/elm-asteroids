module Main exposing (main, viewPaths, transformPoints, wrapPosition)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Random.Pcg as Random
import Time exposing (Time)


-- project modules

import Asteroid exposing (Asteroid)
import Geometry.Circle as Circle
import Geometry.Force as Force
import Geometry.Line as Line exposing (Intersection(SegmentSegment))
import Geometry.Matrix as Matrix exposing (Matrix)
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Screen
import Types exposing (Radians, Polyline, Positioned, Moving, Expiring)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
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



-- model


type alias Model =
    { asteroids : List Asteroid
    , player : Player
    , blaster : Blaster
    , blasts : List Blast
    , controls : Controls
    }


type alias Player =
    Positioned (Moving { polylines : List Polyline })


{-| Represents time until next fire.
-}
type alias Blaster =
    Maybe Time


type alias Blast =
    Expiring
        { position : Point
        , velocity : Vector
        , deltaTime : Float
        }


type alias Controls =
    { left : Bool
    , right : Bool
    , thrust : Bool
    , fire : Bool
    }


initialModel : Model
initialModel =
    { asteroids =
        Random.initialSeed 3780540833
            |> Random.step (Asteroid.field screenSize (screenWidth / 6) 10)
            |> Tuple.first
            |> Force.separate
    , player =
        { polylines = spaceship
        , position = screenSize |> Vector.scale 0.5
        , rotation = pi
        , velocity = Vector.zero
        , angularVelocity = 0
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

                asteroidsNext =
                    model.asteroids |> List.map (updateMoving dt >> wrapPosition)

                ( blastsNext2, asteroidsNext2 ) =
                    interactBlastsAsteroids blastsNext asteroidsNext
            in
                { model
                    | asteroids = asteroidsNext2
                    , player = playerNext
                    , blaster = blasterNext
                    , blasts = blastsNext2
                }


fireBlast : Time -> Player -> Time -> Maybe Blast
fireBlast dt player timeTilFire =
    if timeTilFire < 0.001 then
        let
            -- px / second
            speed =
                Vector.length player.velocity + 800
        in
            { position = player.position
            , velocity = ( speed, player.rotation + pi / 2 ) |> fromPolar
            , timeRemaining = screenWidth / speed
            , deltaTime = dt
            }
                |> updateBlast dt
    else
        Nothing


updateBlast : Time -> Blast -> Maybe Blast
updateBlast dt blast =
    if blast.timeRemaining > 0 then
        Just
            { blast
                | position = blast.position |> Vector.add (blast.velocity |> Vector.scale dt)
                , timeRemaining = blast.timeRemaining - dt
                , deltaTime = dt
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
    { thrustRadians = 1.6 -- rad / second
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
                + (entity.angularVelocity * rotationFriction * dt)
                + rotationThrust

        positionThrust =
            if controls.thrust then
                ( thrustDistance * dt, rotationNext + pi / 2 ) |> fromPolar
            else
                Vector.zero

        positionNext =
            entity.position
                |> Vector.add (entity.velocity |> Vector.scale (positionFriction * dt))
                |> Vector.add (positionThrust)
    in
        { entity
            | position = positionNext
            , rotation = rotationNext
            , velocity = Vector.sub positionNext entity.position |> Vector.scale (1 / dt)
            , angularVelocity = (rotationNext - entity.rotation) / dt
        }


updateMoving : Time -> Moving (Positioned a) -> Moving (Positioned a)
updateMoving dt obj =
    { obj
        | position = obj.position |> Vector.add (obj.velocity |> Vector.scale dt)
        , rotation = obj.rotation + obj.angularVelocity * dt
    }



-- interactions


interactBlastsAsteroids : List Blast -> List Asteroid -> ( List Blast, List Asteroid )
interactBlastsAsteroids blasts asteroids =
    List.foldl
        (interactBlastAsteroids [])
        ( [], asteroids )
        blasts


interactBlastAsteroids : List Asteroid -> Blast -> ( List Blast, List Asteroid ) -> ( List Blast, List Asteroid )
interactBlastAsteroids asteroidsResult blast ( blastsResult, asteroids ) =
    case asteroids of
        [] ->
            ( blast :: blastsResult, asteroidsResult )

        asteroid :: asteroidsRest ->
            case interactBlastAsteroid blast asteroid of
                Just asteroidDamage ->
                    ( blastsResult, asteroidsResult ++ asteroidDamage ++ asteroidsRest )

                Nothing ->
                    interactBlastAsteroids (asteroid :: asteroidsResult) blast ( blastsResult, asteroidsRest )


interactBlastAsteroid : Blast -> Asteroid -> Maybe (List Asteroid)
interactBlastAsteroid blast asteroid =
    if Vector.distanceSquared blast.position asteroid.position < asteroid.radius ^ 2 then
        let
            asteroidPolygon =
                asteroid |> transformAsteroid

            ( a, b ) =
                ( blast |> blastTrailPosition, blast.position )
        in
            impactPoint a b asteroidPolygon
                |> Maybe.map
                    (\impact ->
                        let
                            blastDirection =
                                blast.velocity |> Vector.normalize

                            forceSpeed =
                                (blast.velocity |> Vector.length)
                                    * (blastMass / (blastMass + asteroid.radius ^ 2))
                        in
                            Polygon.split a b asteroidPolygon
                                |> List.map
                                    (\fragment ->
                                        let
                                            ( fragmentPosition, fragmentRadius ) =
                                                fragment |> Circle.enclose

                                            forceDirection =
                                                Vector.direction impact fragmentPosition
                                        in
                                            { polygon = fragment |> List.map ((flip Vector.sub) fragmentPosition)
                                            , radius = fragmentRadius
                                            , position = fragmentPosition
                                            , rotation = 0
                                            , velocity =
                                                asteroid.velocity
                                                    |> Vector.add (forceDirection |> Vector.scale forceSpeed)
                                            , angularVelocity =
                                                asteroid.angularVelocity
                                                    + (angleFrom blastDirection forceDirection)
                                            }
                                    )
                    )
    else
        Nothing


blastMass : Float
blastMass =
    100


{-| Directed angle; assumes unit vectors.
-}
angleFrom : Vector -> Vector -> Float
angleFrom a b =
    atan2 (Vector.cross a b) (Vector.dot a b)


impactPoint : Point -> Point -> Polygon -> Maybe Point
impactPoint a b polygon =
    let
        intersections =
            Polygon.fold
                (Line.intersect SegmentSegment a b >>> unwrap identity (::))
                []
                polygon
    in
        case intersections of
            [] ->
                Nothing

            [ p ] ->
                Just p

            [ p, q ] ->
                Just ((a < b |> either min max) p q)

            points ->
                (a < b |> either List.minimum List.maximum) points



-- view helpers


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
        |> List.map (List.map (Vector.scale 18))


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


( screenWidth, screenHeight ) =
    screenSize


wrapPosition : { a | position : Point } -> { a | position : Point }
wrapPosition object =
    { object
        | position = object.position |> wrapPoint
    }


wrapPoint : Point -> Point
wrapPoint ( x, y ) =
    ( floatModulo x screenWidth
    , floatModulo y screenHeight
    )


floatModulo : Float -> Float -> Float
floatModulo x y =
    let
        n =
            x / y |> floor |> toFloat
    in
        x - n * y



-- view


view : Model -> Html a
view { asteroids, player, blasts } =
    [ asteroids
        |> List.map (transformAsteroid >> (,) True)
    , player.polylines
        |> List.map (transformPoints player.position player.rotation >> (,) False)
    , blasts
        |> List.map (blastToLine >> (,) False)
    ]
        |> List.concat
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


transformPoints : Point -> Radians -> List Point -> List Point
transformPoints position rotation =
    List.map
        (Matrix.transform
            (Matrix.init 1 rotation position)
        )


blastTrailPosition : Blast -> Point
blastTrailPosition { position, velocity, deltaTime } =
    Vector.sub position (velocity |> Vector.scale (deltaTime * 1.1))


blastToLine : Blast -> Polyline
blastToLine blast =
    [ blastTrailPosition blast
    , blast.position
    ]


transformAsteroid : Asteroid -> Polygon
transformAsteroid { polygon, position, rotation } =
    polygon |> transformPoints position rotation



-- helpers


either : a -> a -> Bool -> a
either t f x =
    if x then
        t
    else
        f


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default


(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f g x y =
    g (f x y)
