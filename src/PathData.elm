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


bezierToPolyline : Float -> List Point -> List Point
bezierToPolyline segmentLength controlPoints =
    let
        hullLength =
            controlPoints |> foldPairs (\a b length -> Vector.distance a b + length) 0

        n =
            hullLength / segmentLength |> ceiling
    in
    List.range 0 n
        |> List.map
            (\i -> pointOnBezier (toFloat i / toFloat n) controlPoints)


pointOnBezier : Float -> List Point -> Point
pointOnBezier t controlPoints =
    case controlPoints of
        [] ->
            Vector.zero

        [ p ] ->
            p

        _ ->
            pointOnBezier t (mapPairs (Vector.interpolate t) controlPoints)


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
                    (bezierToPolyline segmentLength [ ( x, y ), ( c, d ), ( a, b ), p ] ++ polyline) :: rest

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



-- List helpers


foldPairs : (a -> a -> b -> b) -> b -> List a -> b
foldPairs f result list =
    case list of
        x :: ((y :: _) as rest) ->
            foldPairs f (f x y result) rest

        _ ->
            result


{-| Combine each pair of consecutive elements; the resulting list's length
will be one less than the given list.
-}
mapPairs : (a -> a -> b) -> List a -> List b
mapPairs f =
    foldPairs (\x y r -> f x y :: r) [] >> List.reverse
