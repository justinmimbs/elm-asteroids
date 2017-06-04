module Geometry.Matrix exposing (Matrix, identity, init, scale, rotate, translate, multiply, transform)

import Geometry.Vector exposing (Vector)


{-| Matrix for 2D affine transformations. Matrix `(a, b, c, d, tx, ty)` is
equivalent to the 3x3 matrix:

    a c tx
    b d ty
    0 0 1

-}
type alias Matrix =
    ( Float, Float, Float, Float, Float, Float )


type alias Radians =
    Float


identity : Matrix
identity =
    ( 1, 0, 0, 1, 0, 0 )


init : Float -> Radians -> Vector -> Matrix
init s r ( tx, ty ) =
    let
        sine =
            sin r

        cosine =
            cos r
    in
        ( s * cosine
        , s * sine
        , s * negate sine
        , s * cosine
        , tx
        , ty
        )


scale : Float -> Matrix -> Matrix
scale s ( a, b, c, d, tx, ty ) =
    ( s * a
    , s * b
    , s * c
    , s * d
    , s * tx
    , s * ty
    )


rotate : Radians -> Matrix -> Matrix
rotate r ( a, b, c, d, tx, ty ) =
    let
        sine =
            sin r

        cosine =
            cos r
    in
        ( a * cosine + c * sine
        , b * cosine + d * sine
        , a * negate sine + c * cosine
        , b * negate sine + d * cosine
        , tx
        , ty
        )


translate : Vector -> Matrix -> Matrix
translate ( x, y ) ( a, b, c, d, tx, ty ) =
    ( a
    , b
    , c
    , d
    , tx + x
    , ty + y
    )


multiply : Matrix -> Matrix -> Matrix
multiply ( a, b, c, d, tx, ty ) ( a2, b2, c2, d2, tx2, ty2 ) =
    ( a * a2 + c * b2
    , b * a2 + d * b2
    , a * c2 + c * d2
    , b * c2 + d * d2
    , a * tx2 + c * ty2 + tx
    , b * tx2 + d * ty2 + ty
    )


transform : Matrix -> Vector -> Vector
transform ( a, b, c, d, tx, ty ) =
    \( x, y ) ->
        ( x * a + y * c + tx
        , x * b + y * d + ty
        )
