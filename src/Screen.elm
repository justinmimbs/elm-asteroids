module Screen exposing (Path, render)

import Svg exposing (Svg)
import Svg.Attributes


-- project

import Geometry.Vector exposing (Point)


type alias Path =
    ( Float, Bool, List Point )


screenId : String
screenId =
    "screen"


render : ( Float, Float ) -> List Path -> Svg a
render ( width, height ) paths =
    Svg.svg
        [ Svg.Attributes.class "screen"
        , Svg.Attributes.viewBox ([ 0, 0, width, height ] |> List.map toString |> String.join " ")
        , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
        , Svg.Attributes.width "auto"
        , Svg.Attributes.height "auto"
        ]
        [ Svg.defs
            []
            [ Svg.g
                [ Svg.Attributes.id screenId ]
                (paths |> List.map viewPath)
            ]
        , Svg.g
            []
            (translations |> List.map (viewProjection width height))
        ]


viewPath : ( Float, Bool, List Point ) -> Svg a
viewPath ( opacity, closed, points ) =
    Svg.node
        (if closed then
            "polygon"
         else
            "polyline"
        )
        [ Svg.Attributes.points (points |> pointsToString)
        , Svg.Attributes.opacity (opacity |> toString)
        ]
        []


pointsToString : List Point -> String
pointsToString =
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
