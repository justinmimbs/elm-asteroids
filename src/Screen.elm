module Screen exposing (render)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import Math.Vector3 as Vector3 exposing (Vec3)


render : ( Float, Float ) -> List (List Vec3) -> Html a
render ( width, height ) polylines =
    Svg.svg
        [ Svg.Attributes.width (width |> toString)
        , Svg.Attributes.height (height |> toString)
        ]
        [ Svg.node "style"
            []
            [ Svg.text "@import url(../app/style.css);"
            ]
        , Svg.g
            []
            (polylines |> List.map viewPolyline)
        ]


viewPolyline : List Vec3 -> Svg a
viewPolyline polyline =
    Svg.polyline
        [ Svg.Attributes.points (polyline |> polylineToString)
        ]
        []


polylineToString : List Vec3 -> String
polylineToString =
    List.foldr
        (vecToString >> (++) " " >> (++))
        ""


vecToString : Vec3 -> String
vecToString vec =
    toString (Vector3.getX vec) ++ "," ++ toString (Vector3.getY vec)
