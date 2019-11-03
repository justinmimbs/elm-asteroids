module Util exposing (floatModulo, transformPoints, wrapPosition)

import Geometry.Matrix as Matrix
import Geometry.Vector exposing (Point)


transformPoints : Point -> Float -> List Point -> List Point
transformPoints position rotation =
    List.map
        (Matrix.transform
            (Matrix.init 1 rotation position)
        )


wrapPosition : ( Float, Float ) -> { a | position : Point } -> { a | position : Point }
wrapPosition bounds a =
    { a
        | position = a.position |> wrapPoint bounds
    }


wrapPoint : ( Float, Float ) -> Point -> Point
wrapPoint ( width, height ) ( x, y ) =
    ( floatModulo x width
    , floatModulo y height
    )


floatModulo : Float -> Float -> Float
floatModulo x y =
    let
        n =
            x / y |> floor |> toFloat
    in
    x - n * y
