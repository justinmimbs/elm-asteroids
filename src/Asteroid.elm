module Asteroid exposing (Asteroid, asteroid, field)

import Geometry.Polygon exposing (Polygon)
import Geometry.Vector exposing (Point, distanceSquared)
import Random exposing (Generator)
import Types exposing (Boundaried, Moving, Positioned)


type alias Asteroid =
    Boundaried (Positioned (Moving {}))



-- field


field : ( Float, Float ) -> Float -> Int -> Generator (List Asteroid)
field ( width, height ) exclusionRadius count =
    Random.list
        count
        (Random.map2
            setPosition
            (Random.pair (Random.float 0 width) (Random.float 0 height)
                |> filter
                    (distanceSquared ( width / 2, height / 2 ) >> (<) (exclusionRadius ^ 2))
            )
            asteroid
        )


filter : (a -> Bool) -> Generator a -> Generator a
filter predicate generator =
    Random.andThen
        (\x ->
            if predicate x then
                Random.constant x

            else
                filter predicate generator
        )
        generator


setPosition : x -> { a | position : x } -> { a | position : x }
setPosition x a =
    { a | position = x }



-- asteroid


asteroid : Generator Asteroid
asteroid =
    Random.float 25 55
        |> Random.andThen
            (\radius ->
                Random.map3
                    (\velocity angularVelocity polygon ->
                        { position = ( 0, 0 )
                        , rotation = 0
                        , velocity = velocity
                        , angularVelocity = angularVelocity
                        , radius = radius
                        , polygon = polygon
                        }
                    )
                    (Random.pair (Random.float 10 80) (Random.float 0 (pi * 2)) |> Random.map fromPolar)
                    (Random.float -1 1)
                    (shape radius)
            )



-- shape


shape : Float -> Generator Polygon
shape radius =
    Random.int (radius / 5 |> floor) (radius / 4 |> ceiling)
        |> Random.andThen (\n -> Random.list n (Random.pair (Random.float (radius * 0.6) radius) (Random.float 0.1 1)))
        |> Random.map toShape


toShape : List ( Float, Float ) -> Polygon
toShape list =
    let
        segmentAngle =
            pi * 2 / toFloat (List.length list)
    in
    list
        |> List.indexedMap
            (\i ( radius, scale ) -> ( radius, segmentAngle * toFloat i + segmentAngle * scale ) |> fromPolar)
