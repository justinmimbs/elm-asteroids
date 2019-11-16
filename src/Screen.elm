module Screen exposing (Path, render)

import Geometry.Vector exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes


type alias Path =
    { opacity : Float
    , closed : Bool
    , points : List Point
    }


screenId : String
screenId =
    "screen"


render : ( Float, Float ) -> List Path -> Svg a
render ( width, height ) =
    let
        attributes =
            [ Svg.Attributes.class "screen"
            , Svg.Attributes.viewBox ([ 0, 0, width, height ] |> List.map String.fromFloat |> String.join " ")
            , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
            ]

        projections =
            Svg.g
                []
                (translations |> List.map (viewProjection width height))
    in
    \paths ->
        Svg.svg
            attributes
            [ Svg.defs
                []
                [ Svg.g
                    [ Svg.Attributes.id screenId ]
                    (paths |> List.map viewPath)
                ]
            , projections
            ]


viewPath : Path -> Svg a
viewPath { opacity, closed, points } =
    Svg.node
        (if closed then
            "polygon"

         else
            "polyline"
        )
        [ Svg.Attributes.points (points |> pointsToString)
        , Svg.Attributes.opacity (opacity |> String.fromFloat)
        ]
        []


pointsToString : List Point -> String
pointsToString =
    List.foldl
        (\( x, y ) string ->
            String.fromFloat x ++ "," ++ String.fromFloat y ++ " " ++ string
        )
        ""



-- screen projections (3 x 3 grid)


viewProjection : Float -> Float -> ( Float, Float ) -> Svg a
viewProjection width height ( u, v ) =
    Svg.svg
        [ Svg.Attributes.width (width |> String.fromFloat)
        , Svg.Attributes.height (height |> String.fromFloat)
        , Svg.Attributes.viewBox ([ width * u, height * v, width, height ] |> List.map String.fromFloat |> String.join " ")
        ]
        [ Svg.use [ Svg.Attributes.xlinkHref ("#" ++ screenId) ] []
        ]


translations : List ( Float, Float )
translations =
    cross [ 0, 1, -1 ]


cross : List a -> List ( a, a )
cross list =
    List.concatMap (\x -> List.map (\y -> ( x, y )) list) list
