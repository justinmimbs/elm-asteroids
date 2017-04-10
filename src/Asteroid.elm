module Asteroid exposing (generator)

import Math.Vector3 as Vector3 exposing (Vec3)
import Random.Pcg as Random exposing (Generator)


generator : Generator (List Vec3)
generator =
    Random.int 9 14
        |> Random.andThen (\n -> Random.list n (Random.pair (Random.float 30 50) (Random.float 0.1 1)))
        |> Random.map toAsteroid


toAsteroid : List ( Float, Float ) -> List Vec3
toAsteroid list =
    let
        segmentAngle =
            pi * 2 / toFloat (List.length list)
    in
        list
            |> List.indexedMap
                (\i ( radius, scale ) -> ( radius, segmentAngle * toFloat i + segmentAngle * scale ) |> fromPolar |> toVec3)
            |> closePath


closePath : List a -> List a
closePath list =
    list ++ List.take 1 list



--


toVec3 : ( Float, Float ) -> Vec3
toVec3 ( x, y ) =
    Vector3.vec3 x y 0
