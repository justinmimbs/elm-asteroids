module Asteroid exposing (Asteroid, asteroid, field)

import Math.Vector3 as Vector3 exposing (Vec3)
import Random.Pcg as Random exposing (Generator)


-- project

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
                |> Random.map toVec3
                |> Random.filter
                    (Vector3.distanceSquared (( width / 2, height / 2 ) |> toVec3) >> ((<) (exclusionRadius ^ 2)))
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
                        , position = vec3Zero
                        , rotation = 0
                        , velocity = velocity
                        , rotationInertia = rotationInertia
                        }
                    )
                    (Random.pair (Random.float 10 80) (Random.float 0 (pi * 2)) |> Random.map (fromPolar >> toVec3))
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
                (\i ( radius, scale ) -> ( radius, segmentAngle * toFloat i + segmentAngle * scale ) |> fromPolar |> toVec3)
            |> closePath


closePath : List a -> List a
closePath list =
    list ++ List.take 1 list



--


toVec3 : ( Float, Float ) -> Vec3
toVec3 ( x, y ) =
    Vector3.vec3 x y 0


vec3Zero : Vec3
vec3Zero =
    Vector3.vec3 0 0 0
