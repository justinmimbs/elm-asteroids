module Particle exposing (Particle, burst)

import Random.Pcg as Random exposing (Generator)


-- project

import Geometry.Vector as Vector
import Types exposing (Radians, Polyline, Positioned, Moving, Expiring)


type alias Particle =
    Positioned (Moving (Expiring { polyline : Polyline }))


burst : Generator (List Particle)
burst =
    Random.int 6 18
        |> Random.andThen ((flip Random.list) burstParticle)


burstParticle : Generator Particle
burstParticle =
    Random.map4
        toBurstParticle
        (Random.float pi (negate pi))
        (Random.float 200 500)
        (Random.float 40 160)
        (Random.float (pi * 2) (pi * -2))


toBurstParticle : Radians -> Float -> Float -> Radians -> Particle
toBurstParticle direction speed travel angularVelocity =
    { position = Vector.zero
    , rotation = direction
    , velocity = ( speed, direction ) |> fromPolar
    , angularVelocity = angularVelocity
    , timeRemaining = travel / speed
    , polyline = burstParticleLine
    }


burstParticleLine : Polyline
burstParticleLine =
    [ ( 0, -1 ), ( 0, 1 ) ]
