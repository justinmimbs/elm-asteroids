module Asteroid exposing (Asteroid, asteroid, field)

import Random.Pcg as Random exposing (Generator)


-- project modules

import Geometry.Vector exposing (distanceSquared)
import Types exposing (Polyline, Renderable, Moving)


type alias Asteroid =
    Renderable (Moving { radius : Float })



-- field


field : ( Float, Float ) -> Float -> Int -> Generator (List Asteroid)
field ( width, height ) exclusionRadius count =
    Random.list
        count
        (Random.map2
            setPosition
            (Random.pair (Random.float 0 width) (Random.float 0 height)
                |> Random.filter
                    (distanceSquared ( width / 2, height / 2 ) >> ((<) (exclusionRadius ^ 2)))
            )
            asteroid
        )


setPosition : x -> { a | position : x } -> { a | position : x }
setPosition x a =
    { a | position = x }



-- asteroid


asteroid : Generator Asteroid
asteroid =
    Random.float 30 60
        |> Random.andThen
            (\radius ->
                Random.map3
                    (\velocity rotationInertia polyline ->
                        { radius = radius
                        , polylines = [ polyline ]
                        , position = ( 0, 0 )
                        , rotation = 0
                        , velocity = velocity
                        , rotationInertia = rotationInertia
                        }
                    )
                    (Random.pair (Random.float 10 80) (Random.float 0 (pi * 2)) |> Random.map fromPolar)
                    (Random.float -1 1)
                    (shape radius)
            )



-- shape


shape : Float -> Generator Polyline
shape radius =
    Random.int (radius / 5 |> floor) (radius / 4 |> ceiling)
        |> Random.andThen (\n -> Random.list n (Random.pair (Random.float (radius * 0.6) radius) (Random.float 0.1 1)))
        |> Random.map toShape


toShape : List ( Float, Float ) -> Polyline
toShape list =
    let
        segmentAngle =
            pi * 2 / toFloat (List.length list)
    in
        list
            |> List.indexedMap
                (\i ( radius, scale ) -> ( radius, segmentAngle * toFloat i + segmentAngle * scale ) |> fromPolar)
            |> closePath


closePath : List a -> List a
closePath list =
    list ++ List.take 1 list
