module Asteroid exposing (Asteroid, asteroid, field)

import Math.Vector3 as Vector3 exposing (Vec3)
import Random.Pcg as Random exposing (Generator)


-- project

import Types exposing (Polyline, Renderable, Moving)


type alias Asteroid =
    Moving (Renderable {})



-- field


field : ( Float, Float ) -> Int -> Generator (List Asteroid)
field ( width, height ) count =
    Random.list
        count
        (Random.map2
            setPosition
            (Random.pair (Random.float 0 width) (Random.float 0 height) |> Random.map toVec3)
            asteroid
        )


setPosition : x -> { a | position : x } -> { a | position : x }
setPosition x a =
    { a | position = x }



-- asteroid


asteroid : Generator Asteroid
asteroid =
    Random.map3
        (\velocity rotationInertia polyline ->
            { polylines = [ polyline ]
            , position = vec3Zero
            , rotation = 0
            , velocity = velocity
            , rotationInertia = rotationInertia
            }
        )
        (Random.pair (Random.float -60 60) (Random.float -60 60) |> Random.map toVec3)
        (Random.float -1 1)
        shape



-- shape


shape : Generator Polyline
shape =
    Random.int 9 14
        |> Random.andThen (\n -> Random.list n (Random.pair (Random.float 30 50) (Random.float 0.1 1)))
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
