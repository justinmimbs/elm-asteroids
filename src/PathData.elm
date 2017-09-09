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


bezierToPolyline : Int -> List Point -> List Point
bezierToPolyline n controls =
    List.range 0 n
        |> List.map
            (\i -> pointOnBezier (toFloat i / toFloat n) controls)


pointOnBezier : Float -> List Point -> Point
pointOnBezier t controls =
    case controls of
        [] ->
            Vector.zero

        [ p ] ->
            p

        _ ->
            pointOnBezier t (mapPairs (Vector.interpolate t) controls)


toPolylines : Int -> PathData -> List (List Point)
toPolylines n =
    List.foldl
        (\command result ->
            case ( command, result ) of
                ( M x y, _ ) ->
                    [ ( x, y ) ] :: result

                ( L x y, polyline :: rest ) ->
                    (( x, y ) :: polyline) :: rest

                ( C a b c d x y, (p :: polyline) :: rest ) ->
                    (bezierToPolyline n [ ( x, y ), ( c, d ), ( a, b ), p ] ++ polyline) :: rest

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
