module Geometry.Vector exposing (Vector, Point, zero, length, lengthSquared, angle, normalize, negate, scale, add, sub, dot, cross, distance, distanceSquared, direction)


type alias Vector =
    ( Float, Float )


type alias Point =
    Vector


zero : Vector
zero =
    ( 0, 0 )


length : Vector -> Float
length =
    lengthSquared >> sqrt


lengthSquared : Vector -> Float
lengthSquared ( x, y ) =
    x ^ 2 + y ^ 2


angle : Vector -> Float
angle ( x, y ) =
    atan2 y x


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


dot : Vector -> Vector -> Float
dot ( x, y ) ( u, v ) =
    x * u + y * v


cross : Vector -> Vector -> Float
cross ( x, y ) ( u, v ) =
    x * v - y * u


distance : Point -> Point -> Float
distance a b =
    distanceSquared a b |> sqrt


distanceSquared : Point -> Point -> Float
distanceSquared ( x, y ) ( u, v ) =
    (u - x) ^ 2 + (v - y) ^ 2


direction : Point -> Point -> Vector
direction a b =
    sub b a |> normalize
