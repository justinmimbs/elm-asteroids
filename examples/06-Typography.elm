module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes


-- project

import Font exposing (Font)
import Font.Astraea as Astraea
import Geometry.Vector as Vector exposing (Point)
import PathData exposing (PathData)


astraeaPathData =
    { p16 = Astraea.pathData |> Font.scale PathData.scale (1 / 3) |> updateWidth ((+) 1.5)
    , p24 = Astraea.pathData |> Font.scale PathData.scale 0.5 |> updateWidth ((+) 2)
    , p32 = Astraea.pathData |> Font.scale PathData.scale (2 / 3) |> updateWidth ((+) 2)
    , p48 = Astraea.pathData |> updateWidth ((+) 2)
    , p72 = Astraea.pathData |> Font.scale PathData.scale 1.5
    , p96 = Astraea.pathData |> Font.scale PathData.scale 2
    , p144 = Astraea.pathData |> Font.scale PathData.scale 3
    }


astraeaPolylines =
    { p16 = astraeaPathData.p16 |> Font.map (PathData.toPolylines 6)
    , p24 = astraeaPathData.p24 |> Font.map (PathData.toPolylines 8)
    , p32 = astraeaPathData.p32 |> Font.map (PathData.toPolylines 10)
    , p48 = astraeaPathData.p48 |> Font.map (PathData.toPolylines 13)
    , p72 = astraeaPathData.p72 |> Font.map (PathData.toPolylines 16)
    , p96 = astraeaPathData.p96 |> Font.map (PathData.toPolylines 18)
    , p144 = astraeaPathData.p144 |> Font.map (PathData.toPolylines 22)
    }


updateWidth : (b -> b) -> { a | width : b } -> { a | width : b }
updateWidth f a =
    { a | width = f a.width }


sampleText : String
sampleText =
    "0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ"


main : Html a
main =
    Svg.svg
        [ Svg.Attributes.width "1200px"
        , Svg.Attributes.height "1400px"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "gray"
        , Svg.Attributes.strokeWidth "2px"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        ]
        [ Svg.g
            [ Svg.Attributes.transform (translate ( 50, [] |> toOffset ))
            , Svg.Attributes.strokeWidth "1.5px"
            ]
            (viewSample 16 ("ROTATE LEFT RIGHT THRUST FIRE SHIELDS " ++ sampleText))
        , Svg.g
            [ Svg.Attributes.transform (translate ( 50, [ 16 ] |> toOffset ))
            ]
            (viewSample 24 ("PRESS ENTER TO CONTINUE " ++ sampleText))
        , Svg.g
            [ Svg.Attributes.transform (translate ( 50, [ 16, 24 ] |> toOffset )) ]
            (viewSample 32 ("PRESS ENTER TO CONTINUE " ++ sampleText))
        , Svg.g
            [ Svg.Attributes.transform (translate ( 50, [ 16, 24, 32 ] |> toOffset )) ]
            (viewSample 48 ("PRESS ENTER TO CONTINUE " ++ sampleText))
        , Svg.g
            [ Svg.Attributes.transform (translate ( 50, [ 16, 24, 32, 48 ] |> toOffset )) ]
            (viewSample 72 ("LEVEL 2 ASTEROIDS " ++ sampleText))
        , Svg.g
            [ Svg.Attributes.transform (translate ( 50, [ 16, 24, 32, 48, 72 ] |> toOffset )) ]
            (viewSample 96 ("LEVEL 2 ASTEROIDS " ++ sampleText))
        , Svg.g
            [ Svg.Attributes.transform (translate ( 50, [ 16, 24, 32, 48, 72, 96 ] |> toOffset )) ]
            (viewSample 144 ("ASTEROIDS " ++ sampleText))
        ]


toOffset : List Float -> Float
toOffset =
    List.foldl ((*) 3 >> (+) 20 >> (+)) 50


viewSample : Int -> String -> List (Svg a)
viewSample pxSize text =
    let
        ( fontPathData, fontPolylines ) =
            [ ( 16, ( astraeaPathData.p16, astraeaPolylines.p16 ) )
            , ( 24, ( astraeaPathData.p24, astraeaPolylines.p24 ) )
            , ( 32, ( astraeaPathData.p32, astraeaPolylines.p32 ) )
            , ( 48, ( astraeaPathData.p48, astraeaPolylines.p48 ) )
            , ( 72, ( astraeaPathData.p72, astraeaPolylines.p72 ) )
            , ( 96, ( astraeaPathData.p96, astraeaPolylines.p96 ) )
            , ( 144, ( astraeaPathData.p144, astraeaPolylines.p144 ) )
            ]
                |> Dict.fromList
                |> Dict.get pxSize
                |> Maybe.withDefault ( astraeaPathData.p16, astraeaPolylines.p16 )
    in
        [ Font.typesetLine ((flip (,)) 0 >> viewPath) fontPathData 0
        , Font.typesetLine (\x p -> p |> List.map (viewPolyline ( x, toFloat pxSize * 1.5 ))) fontPolylines 0 >> List.concat
        ]
            |> List.concatMap ((|>) (toString pxSize ++ " " ++ text))


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
