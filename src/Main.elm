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
import Geometry.Matrix as Matrix
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Particle exposing (Particle)
import Physics
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
    , player : Maybe Player
    , blasts : List Blast
    , particles : List Particle
    , seed : Random.Seed
    , controls : Controls
    }


type alias Player =
    Positioned
        (Moving
            { radius : Float
            , polygon : Polygon
            , polyline : Polyline
            , blaster : Blaster
            }
        )


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
            Just
                { position = screenSize |> Vector.scale 0.5
                , rotation = 0
                , velocity = Vector.zero
                , angularVelocity = 0
                , radius = spaceship.radius
                , polygon = spaceship.hull
                , polyline = spaceship.interior
                , blaster = Nothing
                }
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
                playerU =
                    model.player |> Maybe.map (updatePlayer dt controls >> wrapPosition)

                blastsU =
                    playerU
                        |> Maybe.andThen
                            (\player -> player.blaster |> Maybe.andThen (fireBlast dt player))
                        |> unwrap model.blasts ((flip (::)) model.blasts)
                        |> List.filterMap (updateBlast dt)
                        |> List.map wrapPosition

                asteroidsU =
                    model.asteroids |> List.map (updateMoving dt >> wrapPosition)

                particlesU =
                    model.particles |> List.filterMap (updateMoving dt >> wrapPosition >> updateExpiring dt)

                ( blastsI1, asteroidsI1, maybeParticles1 ) =
                    interactBlastsAsteroids blastsU asteroidsU

                ( blastsI2, playerI1, maybeParticles2 ) =
                    playerU |> unwrap ( blastsI1, Nothing, Nothing ) (interactBlastsPlayer blastsI1)

                ( asteroidsI2, playerI2, maybeParticles3 ) =
                    playerI1 |> unwrap ( asteroidsI1, Nothing, Nothing ) (interactAsteroidsPlayer asteroidsI1)

                ( newParticles, seedNext ) =
                    [ maybeParticles1, maybeParticles2, maybeParticles3 ]
                        |> List.foldl (appendMaybe (Random.map2 (++))) Nothing
                        |> unwrap ( [], model.seed ) ((flip Random.step) model.seed)
            in
                { model
                    | asteroids = asteroidsI2
                    , player = playerI2
                    , blasts = blastsI2
                    , particles = newParticles ++ particlesU
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
            , velocity = ( speed, player.rotation + pi / -2 ) |> fromPolar
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
updatePlayer dt controls player =
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
            player.rotation
                + (player.angularVelocity * rotationFriction * dt)
                + rotationThrust

        positionThrust =
            if controls.thrust then
                ( thrustDistance * dt, rotationNext + pi / -2 ) |> fromPolar
            else
                Vector.zero

        positionNext =
            player.position
                |> Vector.add (player.velocity |> Vector.scale (positionFriction * dt))
                |> Vector.add (positionThrust)
    in
        { player
            | position = positionNext
            , rotation = rotationNext
            , velocity = Vector.sub positionNext player.position |> Vector.scale (1 / dt)
            , angularVelocity = (rotationNext - player.rotation) / dt
            , blaster = player.blaster |> updateBlaster dt controls.fire
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
interactBlastAsteroid =
    interactBlastCollidable
        (\impact asteroid ->
            asteroid
                |> transformPolygon
                |> Polygon.split (impact.blast |> blastTrailPosition) impact.blast.position
                |> List.foldl
                    (\fragment ( fragments, particles ) ->
                        let
                            ( fragmentPosition, fragmentRadius ) =
                                fragment |> Circle.enclose

                            fragmentPolygon =
                                fragment |> List.map ((flip Vector.sub) fragmentPosition)

                            forceVelocity =
                                impact.blast.velocity |> Vector.normalize |> Vector.scale impact.forceSpeed

                            ( fragmentVelocity, fragmentAngularVelocity ) =
                                Physics.impulse forceVelocity impact.point fragmentPosition
                        in
                            if fragmentRadius < 20 then
                                ( fragments
                                , fragmentPolygon
                                    |> Particle.explode impact.forceSpeed impact.forceSpeed
                                    |> Random.map (List.map (adjustParticle fragmentPosition fragmentVelocity))
                                    |> Random.map2 (++) particles
                                )
                            else
                                ( { polygon = fragmentPolygon
                                  , radius = fragmentRadius
                                  , position = fragmentPosition
                                  , rotation = 0
                                  , velocity = asteroid.velocity |> Vector.add fragmentVelocity
                                  , angularVelocity = asteroid.angularVelocity + fragmentAngularVelocity
                                  }
                                    :: fragments
                                , particles
                                )
                    )
                    ( [], impact.particles )
        )


blastMass : Float
blastMass =
    100



--


type alias BlastPlayerResult =
    ( List Blast, Maybe Player, Maybe (Generator (List Particle)) )


interactBlastsPlayer : List Blast -> Player -> BlastPlayerResult
interactBlastsPlayer blasts player =
    List.foldl
        (\blast ( blastsResult, maybePlayer, maybeParticles ) ->
            case maybePlayer |> Maybe.andThen (interactBlastPlayer blast) of
                Just particles ->
                    ( blastsResult, Nothing, Just particles )

                Nothing ->
                    ( blast :: blastsResult, maybePlayer, maybeParticles )
        )
        ( [], Just player, Nothing )
        blasts


interactBlastPlayer : Blast -> Player -> Maybe (Generator (List Particle))
interactBlastPlayer =
    interactBlastCollidable
        (\impact player ->
            Random.map2 (++)
                (Particle.explode impact.forceSpeed impact.forceSpeed (player.polygon))
                (Particle.explode impact.forceSpeed impact.forceSpeed (player.polyline))
                |> Random.map (List.map (adjustParticle player.position player.velocity))
                |> Random.map2 (++) impact.particles
        )



--


type alias Collidable a =
    Positioned (Moving { a | polygon : Polygon, radius : Float })


type alias BlastImpact =
    { blast : Blast
    , point : Point
    , forceSpeed : Float
    , particles : Generator (List Particle)
    }


interactBlastCollidable : (BlastImpact -> Collidable a -> b) -> Blast -> Collidable a -> Maybe b
interactBlastCollidable f blast obj =
    if Vector.distanceSquared blast.position obj.position < obj.radius ^ 2 then
        let
            objPolygon =
                obj |> transformPolygon

            ( a, b ) =
                ( blast |> blastTrailPosition, blast.position )
        in
            impactPoint a b objPolygon
                |> Maybe.map
                    (\point ->
                        f
                            { blast = blast
                            , point = point
                            , forceSpeed =
                                (blast.velocity |> Vector.length)
                                    * (blastMass / (blastMass + obj.radius ^ 2))
                            , particles =
                                Particle.burst 100 80 (obj.radius / 4 |> ceiling)
                                    |> Random.map (List.map (adjustParticle point obj.velocity))
                            }
                            obj
                    )
    else
        Nothing


adjustParticle : Point -> Vector -> Particle -> Particle
adjustParticle position velocity particle =
    { particle
        | position = particle.position |> Vector.add position
        , velocity = particle.velocity |> Vector.add velocity
    }


impactPoint : Point -> Point -> Polygon -> Maybe Point
impactPoint a b polygon =
    case polygon |> Polygon.intersectionsWithSegment a b of
        [] ->
            Nothing

        [ p ] ->
            Just p

        [ p, q ] ->
            Just ((a < b |> either min max) p q)

        points ->
            (a < b |> either List.minimum List.maximum) points



--


type alias AsteroidPlayerResult =
    ( List Asteroid, Maybe Player, Maybe (Generator (List Particle)) )


interactAsteroidsPlayer : List Asteroid -> Player -> AsteroidPlayerResult
interactAsteroidsPlayer asteroids player =
    List.foldl
        (\asteroid ( asteroidsResult, maybePlayer, maybeParticles ) ->
            case maybePlayer |> Maybe.andThen (interactAsteroidPlayer asteroid) of
                Just ( asteroid2, particles ) ->
                    ( asteroid2 :: asteroidsResult, Nothing, Just particles )

                Nothing ->
                    ( asteroid :: asteroidsResult, maybePlayer, maybeParticles )
        )
        ( [], Just player, Nothing )
        asteroids


interactAsteroidPlayer : Asteroid -> Player -> Maybe ( Asteroid, Generator (List Particle) )
interactAsteroidPlayer asteroid player =
    if Vector.distance asteroid.position player.position < asteroid.radius + player.radius then
        let
            intersections =
                Polygon.intersectionsWithPolygon
                    (transformPolygon player)
                    (transformPolygon asteroid)
        in
            if intersections |> Debug.log "int" |> List.isEmpty then
                Nothing
            else
                Just ( asteroid, Random.constant [] )
    else
        Nothing



-- view helpers


spaceship : { hull : Polygon, interior : Polyline, radius : Float }
spaceship =
    { hull =
        [ ( -10, 19 )
        , ( -18, 9 )
        , ( -6, 3 )
        , ( 0, -21 )
        , ( 6, 3 )
        , ( 18, 9 )
        , ( 10, 19 )
        ]
    , interior =
        [ ( -10, 19 )
        , ( -6, 3 )
        , ( 0, 0 )
        , ( 6, 3 )
        , ( 10, 19 )
        ]
    , radius =
        22
    }


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
        |> List.map (transformPolygon >> (,) True)
    , player
        |> unwrap [] playerToPaths
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


playerToPaths : Player -> List Screen.Path
playerToPaths { polygon, polyline, position, rotation } =
    let
        transform =
            transformPoints position rotation
    in
        [ ( True, polygon |> transform )
        , ( False, polyline |> transform )
        ]


transformPolygon : Positioned { a | polygon : Polygon } -> Polygon
transformPolygon { polygon, position, rotation } =
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
