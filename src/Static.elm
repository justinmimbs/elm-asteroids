module Static exposing (instructions)

import Svg exposing (Svg)
import Svg.Attributes


-- project

import Font exposing (Font)
import Font.Astraea as Astraea
import Geometry.Vector as Vector exposing (Point)
import PathData exposing (PathData, Command(M, L))
import Screen


instructions : Svg a
instructions =
    let
        sectionWidth =
            96

        iconSize =
            16

        sections =
            [ ( 'S' |> charIcon astraea16, "SHIELDS", "" )
            , ( 'F' |> charIcon astraea16, "FIRE", "" )
            , ( arrowLeft, "ROTATE", "LEFT" )
            , ( arrowUp, "THRUST", "" )
            , ( arrowRight, "ROTATE", "RIGHT" )
            ]
    in
        Svg.svg
            [ Svg.Attributes.class "instructions"
            , Svg.Attributes.viewBox ([ 0, 0, sectionWidth * (List.length sections + 2), 180 ] |> List.map toString |> String.join " ")
            , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
            , Svg.Attributes.width "auto"
            , Svg.Attributes.height "auto"
            ]
            (sections
                |> List.indexedMap
                    (\i ( keyIcon, line1, line2 ) ->
                        Svg.g
                            [ Svg.Attributes.transform (translate ( sectionWidth * 1.5 + toFloat i * sectionWidth + 0.5, iconSize * 2 + 0.5 ))
                            ]
                            (viewSection iconSize keyIcon line1 line2)
                    )
            )


viewSection : Float -> PathData -> String -> String -> List (Svg a)
viewSection iconSize keyIcon line1 line2 =
    let
        keyPadding =
            8

        keySize =
            iconSize + keyPadding * 2
    in
        [ roundRect ( keySize / -2, 0 ) keySize keySize 4
        , keyIcon |> viewPath ( keySize / -2 + keyPadding, keyPadding )
        , Svg.g
            []
            ((line1 |> typeset astraea16 ( 0, keySize + 2 * astraea16.height ))
                ++ (line2 |> typeset astraea16 ( 0, keySize + 3.5 * astraea16.height ))
            )
        ]


viewPath : Point -> PathData -> Svg a
viewPath offset d =
    Svg.path
        [ Svg.Attributes.d (d |> PathData.toString)
        , Svg.Attributes.transform (translate offset)
        ]
        []


translate : Point -> String
translate ( x, y ) =
    "translate(" ++ toString x ++ ", " ++ toString y ++ ")"


roundRect : Point -> Float -> Float -> Float -> Svg a
roundRect ( x, y ) width height r =
    Svg.rect
        [ Svg.Attributes.x (x |> toString)
        , Svg.Attributes.y (y |> toString)
        , Svg.Attributes.width (width |> toString)
        , Svg.Attributes.height (height |> toString)
        , Svg.Attributes.rx (r |> toString)
        , Svg.Attributes.ry (r |> toString)
        ]
        []



-- typography


astraea16 : Font PathData
astraea16 =
    Astraea.pathData |> Font.scale PathData.scale (1 / 3) |> updateWidth ((+) 1.5)


updateWidth : (b -> b) -> { a | width : b } -> { a | width : b }
updateWidth f a =
    { a | width = f a.width }


typeset : Font PathData -> Point -> String -> List (Svg a)
typeset font ( cx, cy ) text =
    let
        ( ox, oy ) =
            ( cx - (text |> String.length |> toFloat |> (*) font.width) / 2
            , cy - font.height
            )
    in
        text |> Font.typesetLine ((flip (,)) oy >> viewPath) font ox



-- icons


charIcon : Font PathData -> Char -> PathData
charIcon font char =
    Font.getCharacter char font |> PathData.mapPoints (Vector.add ( (font.height - font.width) / 2, 0 ))


{-| 16 x 16
-}
arrowRight : PathData
arrowRight =
    [ M 11 2, L 16 8, L 11 14, M 0 8, L 16 8 ]


arrowLeft : PathData
arrowLeft =
    arrowRight |> PathData.mapPoints (flipHorizontal >> Vector.add ( 16, 0 ))


arrowUp : PathData
arrowUp =
    arrowRight |> PathData.mapPoints (rotateLeft >> Vector.add ( 0, 16 ))


rotateLeft : Point -> Point
rotateLeft ( x, y ) =
    ( y, negate x )


flipHorizontal : Point -> Point
flipHorizontal ( x, y ) =
    ( negate x, y )
