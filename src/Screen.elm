module Screen exposing (render)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes


-- project

import Types exposing (Point)


screenId : String
screenId =
    "screen"


render : ( Float, Float ) -> List (List Point) -> Html a
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


viewPolyline : List Point -> Svg a
viewPolyline polyline =
    Svg.polyline
        [ Svg.Attributes.points (polyline |> polylineToString)
        ]
        []


polylineToString : List Point -> String
polylineToString =
    List.foldr
        (pointToString >> (++) " " >> (++))
        ""


pointToString : Point -> String
pointToString ( x, y ) =
    toString x ++ "," ++ toString y



-- screen projections (3 x 3 grid)


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
