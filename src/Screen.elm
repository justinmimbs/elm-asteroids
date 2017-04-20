module Screen exposing (render)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import Math.Vector3 as Vector3 exposing (Vec3)


screenId : String
screenId =
    "screen"


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
        , Svg.defs
            []
            [ Svg.g
                [ Svg.Attributes.id screenId ]
                (polylines |> List.map viewPolyline)
            ]
        , Svg.g
            []
            (translations |> List.map (viewProjection width height))
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



-- screen projections (3 x 3)


viewProjection : Float -> Float -> ( Float, Float ) -> Svg a
viewProjection width height ( u, v ) =
    Svg.svg
        [ Svg.Attributes.width (width |> toString)
        , Svg.Attributes.height (height |> toString)
        , Svg.Attributes.viewBox ([ width * u, height * v, width, height ] |> List.map toString |> String.join " ")
        ]
        [ Svg.use [ Svg.Attributes.xlinkHref ("#" ++ screenId) ] []
        ]


translations : List ( Float, Float )
translations =
    List.map (,) [ 0, 1, -1 ]
        |> apply [ 0, 1, -1 ]


apply : List a -> List (a -> b) -> List b
apply =
    List.concatMap << (flip List.map)
