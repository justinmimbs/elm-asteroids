module Typography exposing (main)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes


-- project

import PathData exposing (PathData, Command(M, L, C))
import Geometry.Vector as Vector exposing (Point)


type alias Circle =
    ( Point, Float )


d2 : PathData
d2 =
    [ M 5 8, C 6 4 10 0 16 0, C 24 0 28 6 28 12, C 28 18 25 21 18 30, L 4 48, L 30 48 ]


main : Html a
main =
    Svg.svg
        [ Svg.Attributes.width "1000px"
        , Svg.Attributes.height "1000px"
        ]
        [ Svg.g
            [ Svg.Attributes.transform "translate(50, 50) scale(2)"
            ]
            [ Svg.path
                [ Svg.Attributes.d (d2 |> PathData.toString)
                , Svg.Attributes.stroke "gray"
                , Svg.Attributes.strokeWidth "2px"
                , Svg.Attributes.fill "none"
                ]
                []
            ]
        , Svg.g
            [ Svg.Attributes.transform "translate(50, 250) scale(2)"
            ]
            (d2 |> PathData.toPolylines 2 |> List.map viewPolyline)
        ]


viewPolyline : List Point -> Svg a
viewPolyline points =
    Svg.polyline
        [ Svg.Attributes.points (points |> List.map pointToString |> String.join " ")
        , Svg.Attributes.stroke "gray"
        , Svg.Attributes.strokeWidth "2px"
        , Svg.Attributes.fill "none"
        ]
        []


pointToString : Point -> String
pointToString ( x, y ) =
    toString x ++ ", " ++ toString y
