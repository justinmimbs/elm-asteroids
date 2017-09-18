module PathData exposing (PathData, Command(..), toString, toPolylines, fromPolylines, scale, translate, mapPoints)

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


fromPolylines : List (List Point) -> PathData
fromPolylines =
    List.foldr
        (\polyline d ->
            case polyline of
                ( mx, my ) :: ls ->
                    M mx my :: (ls |> List.map lineTo) ++ d

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
moveTo =
    uncurry M


lineTo : Point -> Command
lineTo =
    uncurry L


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
    foldPairs (\x y -> f x y |> (::)) [] >> List.reverse



--


(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f g x y =
    g (f x y)
