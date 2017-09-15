module PathData exposing (PathData, Command(..), toString, toPolylines)

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
            "M" :: ([ x, y ] |> List.map Basics.toString)

        L x y ->
            "L" :: ([ x, y ] |> List.map Basics.toString)

        C a b c d x y ->
            "C" :: ([ a, b, c, d, x, y ] |> List.map Basics.toString)
    )
        |> String.join " "


bezierToPolyline : Float -> List Point -> List Point
bezierToPolyline segmentLength controlPoints =
    let
        hullLength =
            controlPoints |> foldPairs (Vector.distance >>> (+)) 0

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
    foldPairs (\x y -> f x y |> (::)) [] >> List.reverse



--


(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f g x y =
    g (f x y)
