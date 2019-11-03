module Particle exposing (Particle, burst, explode)

import Geometry.Line as Line
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector
import Random exposing (Generator)
import Types exposing (Expiring, Moving, Polyline, Positioned, Radians)


type alias Particle =
    Positioned (Moving (Expiring { polyline : Polyline }))



-- burst


burst : Float -> Float -> Int -> Generator (List Particle)
burst speed radius n =
    burstRadial speed radius n
        |> perturbParticles (speed * 0.9) (radius * 0.9) pi (pi * 3)


burstRadial : Float -> Float -> Int -> List Particle
burstRadial speed radius n =
    let
        sectionAngle =
            (pi * 2) / toFloat n
    in
    List.range 1 n
        |> List.map (\i -> burstRadialParticle speed radius (toFloat (i |> remainderBy 5) * 0.5 + 0.5) (toFloat i * sectionAngle))


burstRadialParticle : Float -> Float -> Float -> Radians -> Particle
burstRadialParticle speed distance halfLength angle =
    { position = Vector.zero
    , rotation = angle
    , velocity = ( speed, angle ) |> fromPolar
    , angularVelocity = 0
    , timeRemaining = distance / speed
    , polyline = [ ( negate halfLength, 0 ), ( halfLength, 0 ) ]
    }



-- explode


explode : Float -> Float -> Polygon -> Generator (List Particle)
explode speed radius polygon =
    explodePolygon speed radius polygon
        |> perturbParticles (speed * 0.5) (radius * 0.5) 1 pi


explodePolygon : Float -> Float -> Polygon -> List Particle
explodePolygon speed radius polygon =
    polygon
        |> Polygon.toSegments
        |> List.map
            (\( a, b ) ->
                let
                    center =
                        Line.midpoint a b
                in
                { polyline = [ a, b ] |> List.map (Vector.sub center)
                , position = center
                , rotation = 0
                , velocity = ( speed, Vector.angle center ) |> fromPolar
                , angularVelocity = 0
                , timeRemaining = radius / speed
                }
            )



-- perturbation


type alias Perturbation =
    { speed : Float
    , distance : Float
    , angle : Radians
    , angularVelocity : Radians
    }


{-| Generate a random perturbation within the ranges provided (plus/minus
each value).
-}
perturbation : Float -> Float -> Radians -> Radians -> Generator Perturbation
perturbation a b c d =
    Random.map4
        Perturbation
        (Random.float (negate a) a)
        (Random.float (negate b) b)
        (Random.float (negate c) c)
        (Random.float (negate d) d)


perturb : Perturbation -> Particle -> Particle
perturb { speed, distance, angle, angularVelocity } particle =
    { particle
        | velocity = particle.velocity |> toPolar |> Vector.add ( speed, angle ) |> fromPolar
        , angularVelocity = particle.angularVelocity + angularVelocity
        , timeRemaining = particle.timeRemaining + distance / (Vector.length particle.velocity + speed)
    }


perturbParticles : Float -> Float -> Radians -> Radians -> List Particle -> Generator (List Particle)
perturbParticles speed distance angle angularVelocity particles =
    Random.map2
        (List.map2 perturb)
        (Random.list (List.length particles) (perturbation speed distance angle angularVelocity))
        (Random.constant particles)
