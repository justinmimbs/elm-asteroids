module Geometry.Matrix exposing (Matrix, identity, init, multiply, rotate, scale, transform, translate)

import Geometry.Vector exposing (Vector)


{-| Matrix for 2D affine transformations. Matrix `(a, b, c, d, tx, ty)` is
equivalent to the 3x3 matrix:

        a c tx
        b d ty
        0 0 1

-}
type alias Matrix =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , tx : Float
    , ty : Float
    }


type alias Radians =
    Float


identity : Matrix
identity =
    Matrix 1 0 0 1 0 0


init : Float -> Radians -> Vector -> Matrix
init s r ( tx, ty ) =
    let
        sine =
            sin r

        cosine =
            cos r
    in
    { a = s * cosine
    , b = s * sine
    , c = s * negate sine
    , d = s * cosine
    , tx = tx
    , ty = ty
    }


scale : Float -> Matrix -> Matrix
scale s { a, b, c, d, tx, ty } =
    { a = s * a
    , b = s * b
    , c = s * c
    , d = s * d
    , tx = s * tx
    , ty = s * ty
    }


rotate : Radians -> Matrix -> Matrix
rotate r { a, b, c, d, tx, ty } =
    let
        sine =
            sin r

        cosine =
            cos r
    in
    { a = a * cosine + c * sine
    , b = b * cosine + d * sine
    , c = a * negate sine + c * cosine
    , d = b * negate sine + d * cosine
    , tx = tx
    , ty = ty
    }


translate : Vector -> Matrix -> Matrix
translate ( x, y ) { a, b, c, d, tx, ty } =
    { a = a
    , b = b
    , c = c
    , d = d
    , tx = tx + x
    , ty = ty + y
    }


multiply : Matrix -> Matrix -> Matrix
multiply { a, b, c, d, tx, ty } m =
    { a = a * m.a + c * m.b
    , b = b * m.a + d * m.b
    , c = a * m.c + c * m.d
    , d = b * m.c + d * m.d
    , tx = a * m.tx + c * m.ty + tx
    , ty = b * m.tx + d * m.ty + ty
    }


transform : Matrix -> Vector -> Vector
transform { a, b, c, d, tx, ty } =
    \( x, y ) ->
        ( x * a + y * c + tx
        , x * b + y * d + ty
        )
