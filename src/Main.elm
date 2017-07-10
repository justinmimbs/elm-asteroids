module Main exposing (main, viewPaths, transformPoints, wrapPosition, updateMoving, updateExpiring)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Keyboard
import Random.Pcg as Random exposing (Generator)
import Time exposing (Time)


-- project modules

import Asteroid exposing (Asteroid)
import Geometry.Circle as Circle
import Geometry.Force as Force
import Geometry.Line as Line exposing (Intersection(SegmentSegment))
import Geometry.Matrix as Matrix exposing (Matrix)
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Particle exposing (Particle)
import Screen
import Types exposing (Radians, Polyline, Positioned, Moving, Expiring)


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



-- model


type alias Model =
    { asteroids : List Asteroid
    , player : Player
    , blaster : Blaster
    , blasts : List Blast
    , particles : List Particle
    , seed : Random.Seed
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


init : Random.Seed -> Model
init seed =
    let
        ( asteroids, seedNext ) =
            seed |> Random.step (Asteroid.field screenSize (screenWidth / 6) 10)
    in
        { asteroids = asteroids |> Force.separate
        , player =
            { polylines = spaceship
            , position = screenSize |> Vector.scale 0.5
            , rotation = pi
            , velocity = Vector.zero
            , angularVelocity = 0
            }
        , blaster = Nothing
        , blasts = []
        , particles = []
        , seed = seedNext
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
                        |> unwrap model.blasts ((flip (::)) model.blasts)
                        |> List.filterMap (updateBlast dt)
                        |> List.map wrapPosition

                asteroidsNext =
                    model.asteroids |> List.map (updateMoving dt >> wrapPosition)

                particlesNext =
                    model.particles |> List.filterMap (updateMoving dt >> wrapPosition >> updateExpiring dt)

                ( blastsNext2, asteroidsNext2, maybeParticles ) =
                    interactBlastsAsteroids blastsNext asteroidsNext

                ( particles, seedNext ) =
                    maybeParticles |> unwrap ( [], model.seed ) ((flip Random.step) model.seed)
            in
                { model
                    | asteroids = asteroidsNext2
                    , player = playerNext
                    , blaster = blasterNext
                    , blasts = blastsNext2
                    , particles = particles ++ particlesNext
                    , seed = seedNext
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


updateExpiring : Time -> Expiring a -> Maybe (Expiring a)
updateExpiring dt obj =
    if obj.timeRemaining > 0 then
        Just
            { obj | timeRemaining = obj.timeRemaining - dt }
    else
        Nothing



-- interactions


type alias BlastAsteroidResult =
    ( List Blast, List Asteroid, Maybe (Generator (List Particle)) )


interactBlastsAsteroids : List Blast -> List Asteroid -> BlastAsteroidResult
interactBlastsAsteroids blasts asteroids =
    List.foldl
        (interactBlastAsteroids [])
        ( [], asteroids, Nothing )
        blasts


interactBlastAsteroids : List Asteroid -> Blast -> BlastAsteroidResult -> BlastAsteroidResult
interactBlastAsteroids asteroidsResult blast ( blastsResult, asteroids, maybeParticles ) =
    case asteroids of
        [] ->
            ( blast :: blastsResult
            , asteroidsResult
            , maybeParticles
            )

        asteroid :: asteroidsRest ->
            case interactBlastAsteroid blast asteroid of
                Just ( asteroidDamage, particles ) ->
                    ( blastsResult
                    , asteroidDamage ++ asteroidsRest ++ asteroidsResult
                    , appendMaybe (Random.map2 (++)) (Just particles) maybeParticles
                    )

                Nothing ->
                    interactBlastAsteroids
                        (asteroid :: asteroidsResult)
                        blast
                        ( blastsResult
                        , asteroidsRest
                        , maybeParticles
                        )


interactBlastAsteroid : Blast -> Asteroid -> Maybe ( List Asteroid, Generator (List Particle) )
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

                            impactParticles =
                                Particle.burst 100 80 (asteroid.radius / 4 |> ceiling)
                                    |> Random.map (List.map (adjustParticle impact asteroid.velocity))
                        in
                            Polygon.split a b asteroidPolygon
                                |> List.foldl
                                    (\fragment ( fragments, particles ) ->
                                        let
                                            ( fragmentPosition, fragmentRadius ) =
                                                fragment |> Circle.enclose

                                            forceDirection =
                                                Vector.direction impact fragmentPosition

                                            fragmentPolygon =
                                                fragment |> List.map ((flip Vector.sub) fragmentPosition)

                                            fragmentVelocity =
                                                asteroid.velocity |> Vector.add (forceDirection |> Vector.scale forceSpeed)
                                        in
                                            if fragmentRadius < 20 then
                                                ( fragments
                                                , Particle.explode forceSpeed forceSpeed fragmentPolygon
                                                    |> Random.map (List.map (adjustParticle fragmentPosition fragmentVelocity))
                                                    |> Random.map2 (++) particles
                                                )
                                            else
                                                ( { polygon = fragmentPolygon
                                                  , radius = fragmentRadius
                                                  , position = fragmentPosition
                                                  , rotation = 0
                                                  , velocity = fragmentVelocity
                                                  , angularVelocity = asteroid.angularVelocity + (angleFrom blastDirection forceDirection)
                                                  }
                                                    :: fragments
                                                , particles
                                                )
                                    )
                                    ( [], impactParticles )
                    )
    else
        Nothing


blastMass : Float
blastMass =
    100


adjustParticle : Point -> Vector -> Particle -> Particle
adjustParticle position velocity particle =
    { particle
        | position = particle.position |> Vector.add position
        , velocity = particle.velocity |> Vector.add velocity
    }


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
view { asteroids, player, blasts, particles } =
    [ asteroids
        |> List.map (transformAsteroid >> (,) True)
    , player.polylines
        |> List.map (transformPoints player.position player.rotation >> (,) False)
    , blasts
        |> List.map (blastToLine >> (,) False)
    , particles
        |> List.map (transformParticle >> (,) False)
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


transformAsteroid : Asteroid -> Polygon
transformAsteroid { polygon, position, rotation } =
    polygon |> transformPoints position rotation


transformParticle : Particle -> Polyline
transformParticle { polyline, position, rotation } =
    polyline |> transformPoints position rotation


blastTrailPosition : Blast -> Point
blastTrailPosition { position, velocity, deltaTime } =
    Vector.sub position (velocity |> Vector.scale (deltaTime * 1.1))


blastToLine : Blast -> Polyline
blastToLine blast =
    [ blastTrailPosition blast
    , blast.position
    ]



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


appendMaybe : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
appendMaybe append mx my =
    case ( mx, my ) of
        ( Just x, Just y ) ->
            Just (append x y)

        ( Just _, Nothing ) ->
            mx

        ( Nothing, Just _ ) ->
            my

        _ ->
            Nothing
