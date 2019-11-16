module PathData exposing (Command(..), PathData, fromPolylines, mapPoints, scale, toPolylines, toString, translate)

import Geometry.Vector as Vector exposing (Point)


type Command
    = M Float Float
    | L Float Float
    | C Float Float Float Float Float Float


type alias PathData =
    List Command


toString : PathData -> String
toString =
    List.map commandToString >> String.join " "


commandToString : Command -> String
commandToString command =
    (case command of
        M x y ->
            "M" :: ([ x, y ] |> List.map String.fromFloat)

        L x y ->
            "L" :: ([ x, y ] |> List.map String.fromFloat)

        C a b c d x y ->
            "C" :: ([ a, b, c, d, x, y ] |> List.map String.fromFloat)
    )
        |> String.join " "


cubicBezierToPolyline : Float -> Point -> Point -> Point -> Point -> List Point
cubicBezierToPolyline segmentLength p1 p2 p3 p4 =
    let
        hullLength =
            Vector.distance p1 p2 + Vector.distance p2 p3 + Vector.distance p3 p4

        n =
            hullLength / segmentLength |> ceiling
    in
    List.map
        (\i -> cubicBezierPoint (toFloat i / toFloat n) p1 p2 p3 p4)
        (List.range 0 n)


cubicBezierPoint : Float -> Point -> Point -> Point -> Point -> Point
cubicBezierPoint t ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) =
    ( (1 - t) ^ 3 * x1 + 3 * (1 - t) ^ 2 * t * x2 + 3 * (1 - t) * t ^ 2 * x3 + t ^ 3 * x4
    , (1 - t) ^ 3 * y1 + 3 * (1 - t) ^ 2 * t * y2 + 3 * (1 - t) * t ^ 2 * y3 + t ^ 3 * y4
    )


toPolylines : Float -> PathData -> List (List Point)
toPolylines segmentLength =
    List.foldl
        (\command result ->
            case ( command, result ) of
                ( M x y, _ ) ->
                    [ ( x, y ) ] :: result

                ( L x y, polyline :: rest ) ->
                    (( x, y ) :: polyline) :: rest

                ( C a b c d x y, (p :: polyline) :: rest ) ->
                    (cubicBezierToPolyline segmentLength ( x, y ) ( c, d ) ( a, b ) p ++ polyline) :: rest

                _ ->
                    result
        )
        []


fromPolylines : List (List Point) -> PathData
fromPolylines =
    List.foldr
        (\polyline d ->
            case polyline of
                ( x, y ) :: points ->
                    M x y :: (points |> List.map lineTo) ++ d

                [] ->
                    d
        )
        []


scale : Float -> PathData -> PathData
scale =
    Vector.scale >> mapPoints


translate : Point -> PathData -> PathData
translate =
    Vector.add >> mapPoints


mapPoints : (Point -> Point) -> PathData -> PathData
mapPoints =
    commandMapPoints >> List.map


commandMapPoints : (Point -> Point) -> Command -> Command
commandMapPoints f command =
    case command of
        M x y ->
            moveTo (f ( x, y ))

        L x y ->
            lineTo (f ( x, y ))

        C a b c d x y ->
            cubicTo (f ( a, b )) (f ( c, d )) (f ( x, y ))


moveTo : Point -> Command
moveTo ( x, y ) =
    M x y


lineTo : Point -> Command
lineTo ( x, y ) =
    L x y


cubicTo : Point -> Point -> Point -> Command
cubicTo ( a, b ) ( c, d ) ( x, y ) =
    C a b c d x y
