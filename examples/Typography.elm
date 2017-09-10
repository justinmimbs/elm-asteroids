module Typography exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes


-- project

import Font exposing (Font)
import Font.Astraea as Astraea
import Geometry.Vector as Vector exposing (Point)
import PathData exposing (PathData, Command(M, L, C))


type alias Circle =
    ( Point, Float )


astraeaPolylines : Font (List (List Point))
astraeaPolylines =
    Astraea.pathData |> Font.map (PathData.toPolylines 3)


main : Html a
main =
    Svg.svg
        [ Svg.Attributes.width "1000px"
        , Svg.Attributes.height "1000px"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "gray"
        , Svg.Attributes.strokeWidth "2px"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        ]
        [ Svg.g
            [ Svg.Attributes.strokeWidth "2px"
            , Svg.Attributes.strokeLinecap "square"
            , Svg.Attributes.strokeLinejoin "miter"
            , Svg.Attributes.strokeMiterlimit "4"
            ]
            ("1234 5678" |> Font.typesetLine ((flip (,)) 50 >> viewPath) Astraea.pathData 50)
        , Svg.g
            []
            ("1234 5678" |> Font.typesetLine (\x p -> p |> List.map (viewPolyline ( x, 150 ))) astraeaPolylines 50 |> List.concat)
        ]


viewPath : Point -> PathData -> Svg a
viewPath offset d =
    Svg.path
        [ Svg.Attributes.d (d |> PathData.toString)
        , Svg.Attributes.transform (translate offset)
        ]
        []


translate : Point -> String
translate offset =
    "translate(" ++ (offset |> pointToString) ++ ")"


viewPolyline : Point -> List Point -> Svg a
viewPolyline offset points =
    Svg.polyline
        [ Svg.Attributes.points (points |> List.map (Vector.add offset >> pointToString) |> String.join " ")
        ]
        []


pointToString : Point -> String
pointToString ( x, y ) =
    toString x ++ ", " ++ toString y
