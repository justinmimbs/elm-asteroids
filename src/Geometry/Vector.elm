module Geometry.Vector exposing (Vector, zero, length, lengthSquared, normalize, negate, scale, add, sub, distance, distanceSquared, direction)


type alias Vector =
    ( Float, Float )


zero : Vector
zero =
    ( 0, 0 )


length : Vector -> Float
length =
    lengthSquared >> sqrt


lengthSquared : Vector -> Float
lengthSquared ( x, y ) =
    x ^ 2 + y ^ 2


normalize : Vector -> Vector
normalize (( x, y ) as vec) =
    let
        len =
            length vec
    in
        ( x / len
        , y / len
        )


negate : Vector -> Vector
negate ( x, y ) =
    ( Basics.negate x
    , Basics.negate y
    )


scale : Float -> Vector -> Vector
scale s ( x, y ) =
    ( s * x
    , s * y
    )


add : Vector -> Vector -> Vector
add ( x, y ) ( u, v ) =
    ( x + u
    , y + v
    )


sub : Vector -> Vector -> Vector
sub ( x, y ) ( u, v ) =
    ( x - u
    , y - v
    )


distance : Vector -> Vector -> Float
distance a b =
    distanceSquared a b |> sqrt


distanceSquared : Vector -> Vector -> Float
distanceSquared ( x, y ) ( u, v ) =
    (u - x) ^ 2 + (v - y) ^ 2


direction : Vector -> Vector -> Vector
direction a b =
    sub b a |> normalize
