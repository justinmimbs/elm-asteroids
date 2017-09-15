module Level exposing (Controls, initialControls, Level, init, update, toPaths)

import Random.Pcg as Random exposing (Generator)
import Time exposing (Time)


-- project modules

import Asteroid exposing (Asteroid)
import Geometry.Circle as Circle
import Geometry.Line as Line exposing (Intersection(SegmentSegment))
import Geometry.Matrix as Matrix
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Particle exposing (Particle)
import Physics exposing (Movement, Collidable)
import Screen
import Types exposing (Radians, Polyline, Boundaried, Positioned, Moving, Expiring)


type alias Controls =
    { left : Bool
    , right : Bool
    , thrust : Bool
    , fire : Bool
    , shield : Bool
    }


initialControls : Controls
initialControls =
    { left = False
    , right = False
    , thrust = False
    , fire = False
    , shield = False
    }



--


type alias Level =
    { screenSize : ( Float, Float )
    , seed : Random.Seed
    , asteroids : List Asteroid
    , player : Maybe Player
    , blasts : List Blast
    , particles : List Particle
    }


type alias Player =
    Positioned
        (Moving
            { spaceship : Spaceship
            , aux : Aux
            }
        )


type alias Spaceship =
    { hull : Polygon
    , interior : Polyline
    , shield : Polygon
    , radius : Float
    }


type Aux
    = Off
    | Firing Charge
    | Shielding Charge


type Charge
    = Charged
    | Charging Time


type alias Blast =
    Expiring
        { position : Point
        , velocity : Vector
        , deltaTime : Float
        }


init : ( Float, Float ) -> Random.Seed -> Level
init screenSize seed =
    let
        ( asteroids, seedNext ) =
            seed |> Random.step (Asteroid.field screenSize (Tuple.first screenSize / 6) 10)
    in
        { screenSize = screenSize
        , seed = seedNext
        , asteroids = asteroids
        , player =
            Just
                { position = screenSize |> Vector.scale 0.5
                , rotation = 0
                , velocity = Vector.zero
                , angularVelocity = 0
                , spaceship = spaceship0
                , aux = Off
                }
        , blasts = []
        , particles = []
        }


spaceship0 : Spaceship
spaceship0 =
    { hull =
        [ ( -10, 19 )
        , ( -18, 9 )
        , ( -6, 3 )
        , ( 0, -21 )
        , ( 6, 3 )
        , ( 18, 9 )
        , ( 10, 19 )
        ]
            |> List.map (Vector.scale (18 / 22))
    , interior =
        [ ( -10, 19 )
        , ( -6, 3 )
        , ( 0, 0 )
        , ( 6, 3 )
        , ( 10, 19 )
        ]
            |> List.map (Vector.scale (18 / 22))
    , shield =
        Polygon.ngon 16 |> List.map (Vector.scale 19)
    , radius =
        18
    }



-- update


update : Time -> Controls -> Level -> Level
update dt controls model =
    let
        wrapPosition_ =
            wrapPosition model.screenSize

        playerU =
            model.player |> Maybe.map (updatePlayer dt controls >> wrapPosition_)

        blastsU =
            playerU
                |> Maybe.andThen (fireBlast dt)
                |> unwrap model.blasts ((flip (::)) model.blasts)
                |> List.filterMap (updateBlast dt)
                |> List.map wrapPosition_

        asteroidsU =
            model.asteroids |> List.map (updateMoving dt >> wrapPosition_)

        particlesU =
            model.particles |> List.filterMap (updateMoving dt >> wrapPosition_ >> updateExpiring dt)

        ( blastsI1, asteroidsI1, maybeParticles1 ) =
            interactBlastsAsteroids blastsU asteroidsU

        ( blastsI2, playerC1, maybeParticles2 ) =
            playerU |> unwrap ( blastsI1, Nothing, Nothing ) (playerToPlayerC >> interactBlastsPlayer blastsI1)

        ( asteroidsI2, playerC2, maybeParticles3 ) =
            playerC1 |> unwrap ( asteroidsI1, Nothing, Nothing ) (interactAsteroidsPlayer asteroidsI1)

        ( newParticles, seedNext ) =
            [ maybeParticles1, maybeParticles2, maybeParticles3 ]
                |> List.foldl (appendMaybe (Random.map2 (++))) Nothing
                |> unwrap ( [], model.seed ) ((flip Random.step) model.seed)
    in
        { model
            | asteroids = asteroidsI2
            , player = Maybe.map playerFromPlayerC playerC2
            , blasts = blastsI2
            , particles = newParticles ++ particlesU
            , seed = seedNext
        }


fireBlast : Time -> Player -> Maybe Blast
fireBlast dt player =
    if player.aux == Firing Charged then
        let
            speed =
                Vector.length player.velocity + 800
        in
            { position = player.position
            , velocity = ( speed, player.rotation + pi / -2 ) |> fromPolar
            , timeRemaining = 1200 / speed
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


playerSettings =
    { turningSpeed = 1.6 -- rad / second
    , thrustSpeed = 35 -- px / second
    , positionFriction = 0.98
    , rotationFriction = 0.8
    }


updatePlayer : Time -> Controls -> Player -> Player
updatePlayer dt controls player =
    let
        { turningSpeed, thrustSpeed, positionFriction, rotationFriction } =
            playerSettings

        rotationThrust =
            case ( controls.left, controls.right ) of
                ( True, False ) ->
                    turningSpeed * dt |> negate

                ( False, True ) ->
                    turningSpeed * dt

                _ ->
                    0

        rotationNext =
            player.rotation
                + (player.angularVelocity * rotationFriction * dt)
                + rotationThrust

        positionThrust =
            if controls.thrust then
                ( thrustSpeed * dt, rotationNext + pi / -2 ) |> fromPolar
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
            , aux = player.aux |> updateAux dt controls
        }


updateAux : Time -> Controls -> Aux -> Aux
updateAux dt controls aux =
    case ( controls.shield, controls.fire ) of
        ( False, False ) ->
            Off

        ( False, True ) ->
            case aux of
                Firing charge ->
                    -- 6 hz
                    Firing (charge |> updateCharge dt (Just (1 / 6)))

                _ ->
                    Firing Charged

        ( True, _ ) ->
            case aux of
                Shielding charge ->
                    Shielding (charge |> updateCharge dt Nothing)

                _ ->
                    Shielding Charged


updateCharge : Time -> Maybe Time -> Charge -> Charge
updateCharge dt cycleTime charge =
    case charge of
        Charging t ->
            if t < 0.001 then
                Charged
            else
                (Charging (t - dt))

        Charged ->
            cycleTime |> unwrap Charged ((+) -dt >> Charging)


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
                            if fragmentRadius < 18 then
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
                                  , velocity =
                                        Vector.interpolate (fragmentRadius ^ 2 / asteroid.radius ^ 2)
                                            (Vector.direction asteroid.position fragmentPosition |> Vector.scale impact.forceSpeed)
                                            asteroid.velocity
                                            |> Vector.add fragmentVelocity
                                  , angularVelocity = asteroid.angularVelocity + fragmentAngularVelocity
                                  }
                                    :: fragments
                                , particles
                                )
                    )
                    ( [], impact.particles )
        )



--


type alias BlastPlayerResult =
    ( List Blast, Maybe PlayerC, Maybe (Generator (List Particle)) )


interactBlastsPlayer : List Blast -> PlayerC -> BlastPlayerResult
interactBlastsPlayer blasts player =
    List.foldl
        (\blast ( blastsResult, maybePlayer, maybeParticles ) ->
            case maybePlayer |> Maybe.andThen (interactBlastPlayer blast) of
                Just ( maybePlayerNext, particles ) ->
                    ( blastsResult, maybePlayerNext, Just particles )

                Nothing ->
                    ( blast :: blastsResult, maybePlayer, maybeParticles )
        )
        ( [], Just player, Nothing )
        blasts


interactBlastPlayer : Blast -> PlayerC -> Maybe ( Maybe PlayerC, Generator (List Particle) )
interactBlastPlayer =
    interactBlastCollidable
        (\impact player ->
            if player.aux == Shielding Charged then
                ( { player | aux = Shielding (Charging (impact.forceSpeed * 0.002)) }
                    |> addMovement
                        (Physics.impulse
                            (impact.blast.velocity |> Vector.normalize |> Vector.scale impact.forceSpeed)
                            -- forceVelocity
                            impact.point
                            player.position
                        )
                    |> Just
                , impact.particles
                )
            else
                ( Nothing
                , player
                    |> explodePlayer impact.forceSpeed
                    |> Random.map2 (++)
                        impact.particles
                )
        )


explodePlayer : Float -> PlayerC -> Generator (List Particle)
explodePlayer speed player =
    Random.map2 (++)
        -- spaceship pieces
        (Random.map2 (++)
            (Particle.explode speed speed (player.spaceship.hull))
            (Particle.explode speed speed (player.spaceship.interior))
            |> Random.map (List.map (adjustParticle player.position player.velocity))
        )
        -- burst
        (Particle.burst 150 120 18
            |> Random.map (List.map (adjustParticle player.position (player.velocity |> Vector.scale 0.5)))
        )



--


type alias AsteroidPlayerResult =
    ( List Asteroid, Maybe PlayerC, Maybe (Generator (List Particle)) )


interactAsteroidsPlayer : List Asteroid -> PlayerC -> AsteroidPlayerResult
interactAsteroidsPlayer asteroids player =
    List.foldl
        (\asteroid ( asteroidsResult, maybePlayer, maybeParticles ) ->
            case maybePlayer |> Maybe.andThen (interactAsteroidPlayer asteroid) of
                Just ( asteroid2, maybePlayerNext, particles ) ->
                    ( asteroid2 :: asteroidsResult, maybePlayerNext, Just particles )

                Nothing ->
                    ( asteroid :: asteroidsResult, maybePlayer, maybeParticles )
        )
        ( [], Just player, Nothing )
        asteroids


interactAsteroidPlayer : Asteroid -> PlayerC -> Maybe ( Asteroid, Maybe PlayerC, Generator (List Particle) )
interactAsteroidPlayer asteroid player =
    if player.aux == Shielding Charged then
        Physics.collide 1 asteroid player
            |> Maybe.map
                (\( aMovement, pMovement, contactPoint ) ->
                    let
                        burstSpeed =
                            Vector.length (Vector.add asteroid.velocity player.velocity) * 0.2 |> max 80

                        t =
                            player.radius ^ 2 / (player.radius ^ 2 + asteroid.radius ^ 2)
                    in
                        ( asteroid |> setMovement aMovement
                        , { player | aux = Shielding (Charging (burstSpeed * 0.002)) } |> setMovement pMovement |> Just
                        , Particle.burst burstSpeed (burstSpeed * 0.2) (sqrt burstSpeed * 0.5 |> ceiling)
                            |> Random.map (List.map (adjustParticle contactPoint (Vector.interpolate t asteroid.velocity player.velocity)))
                        )
                )
    else
        Physics.collide 0.2 asteroid player
            |> Maybe.map
                (\( aMovement, pMovement, _ ) ->
                    ( asteroid |> setMovement aMovement
                    , Nothing
                    , player
                        |> setMovement pMovement
                        |> explodePlayer ((Vector.length asteroid.velocity + Vector.length player.velocity) / 4 + 50)
                    )
                )



-- PlayerC (Player Collidable)


type alias PlayerC =
    Boundaried Player


playerToPlayerC : Player -> PlayerC
playerToPlayerC player =
    { position = player.position
    , rotation = player.rotation
    , velocity = player.velocity
    , angularVelocity = player.angularVelocity
    , spaceship = player.spaceship
    , aux = player.aux

    -- boundaried
    , radius = player.spaceship.radius
    , polygon =
        if player.aux == Shielding Charged then
            player.spaceship.shield
        else
            player.spaceship.hull
    }


playerFromPlayerC : PlayerC -> Player
playerFromPlayerC player =
    { position = player.position
    , rotation = player.rotation
    , velocity = player.velocity
    , angularVelocity = player.angularVelocity
    , spaceship = player.spaceship
    , aux = player.aux
    }



-- blast impact


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
                                Particle.burst 100 50 (obj.radius / 4 |> ceiling)
                                    |> Random.map (List.map (adjustParticle point obj.velocity))
                            }
                            obj
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



-- movement


setMovement : Movement -> Moving a -> Moving a
setMovement ( v, av ) a =
    { a
        | velocity = v
        , angularVelocity = av
    }


addMovement : Movement -> Moving a -> Moving a
addMovement ( v, av ) a =
    { a
        | velocity = a.velocity |> Vector.add v
        , angularVelocity = a.angularVelocity + av
    }



-- wrapping


wrapPosition : ( Float, Float ) -> { a | position : Point } -> { a | position : Point }
wrapPosition screenSize object =
    { object
        | position = object.position |> wrapPoint screenSize
    }


wrapPoint : ( Float, Float ) -> Point -> Point
wrapPoint ( width, height ) ( x, y ) =
    ( floatModulo x width
    , floatModulo y height
    )


floatModulo : Float -> Float -> Float
floatModulo x y =
    let
        n =
            x / y |> floor |> toFloat
    in
        x - n * y



-- view


toPaths : Level -> List Screen.Path
toPaths { asteroids, player, blasts, particles } =
    [ asteroids
        |> List.map (transformPolygon >> (,,) 0.5 True)
    , player
        |> unwrap [] playerToPaths
    , blasts
        |> List.map (blastToLine >> (,,) 1 False)
    , particles
        |> List.map particleToPath
    ]
        |> List.concat


playerToPaths : Player -> List Screen.Path
playerToPaths { position, rotation, spaceship, aux } =
    let
        transform =
            transformPoints position rotation
    in
        [ ( 1, True, spaceship.hull |> transform )
        , ( 1, False, spaceship.interior |> transform )
        ]
            |> (if aux == Shielding Charged then
                    (::) ( 1, True, spaceship.shield |> transform )
                else
                    identity
               )


particleToPath : Particle -> Screen.Path
particleToPath { polyline, position, rotation } =
    ( abs (0.5 - floatModulo (rotation / pi) 1) + 0.5
    , False
    , polyline |> transformPoints position rotation
    )


transformPoints : Point -> Radians -> List Point -> List Point
transformPoints position rotation =
    List.map
        (Matrix.transform
            (Matrix.init 1 rotation position)
        )


transformPolygon : Positioned { a | polygon : Polygon } -> Polygon
transformPolygon { polygon, position, rotation } =
    polygon |> transformPoints position rotation


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
