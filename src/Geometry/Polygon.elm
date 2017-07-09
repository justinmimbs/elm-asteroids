module Geometry.Polygon exposing (Polygon, ngon, fold, toSegments, split)

import Geometry.Line as Line exposing (Intersection(LineSegment))
import Geometry.Vector exposing (Point)


type alias Polygon =
    List Point



-- create


ngon : Int -> Polygon
ngon n =
    let
        sectionAngle =
            (pi * 2) / toFloat n
    in
        List.range 1 n
            |> List.map (toFloat >> (*) sectionAngle >> (,) 1 >> fromPolar)



-- transform


{-| Left fold over each item with the next, and the last item with the first.
This is useful for folding over the segments formed by a list of points.
-}
fold : (a -> a -> b -> b) -> b -> List a -> b
fold f2 result list =
    case list of
        head :: _ ->
            foldHelp head f2 result list

        [] ->
            result


foldHelp : a -> (a -> a -> b -> b) -> b -> List a -> b
foldHelp head f2 result list =
    case list of
        x :: ((y :: _) as rest) ->
            foldHelp head f2 (f2 x y result) rest

        last :: [] ->
            f2 last head result

        [] ->
            result


toSegments : Polygon -> List ( Point, Point )
toSegments =
    fold
        ((,) >>> (::))
        []



-- split


{-| Split a polygon by a line. Assumes polygon is not self-intersecting.
-}
split : Point -> Point -> Polygon -> List Polygon
split a b polygon =
    polygon
        |> splitPoints a b
        |> sortSplitPoints ( [], [] )
        |> fromSplitPoints ( [], [], [] )



-- (1) Insert intersection points


type SplitPoint
    = I Point
    | P Point


{-| (Result is reversed.)
-}
splitPoints : Point -> Point -> Polygon -> List SplitPoint
splitPoints a b =
    fold
        (\p1 p2 result ->
            case Line.intersect LineSegment a b p1 p2 of
                Just p ->
                    I p :: P p1 :: result

                Nothing ->
                    P p1 :: result
        )
        []



-- (2) Cycle list until intersection points are in sorted order


{-| Iterate over the list, tracking the intesection points encountered, until
finding an intersection point opposing the order of the previous 2 intersection
points; this point should be at one of the ends. Sorting is needed only when
there are more than 2 intersection points.
(Result is reversed.)
-}
sortSplitPoints : ( List Point, List SplitPoint ) -> List SplitPoint -> List SplitPoint
sortSplitPoints ( intersections, result ) list =
    case list of
        [] ->
            result

        (P p) :: rest ->
            sortSplitPoints ( intersections, P p :: result ) rest

        (I p) :: rest ->
            case intersections of
                p1 :: p2 :: _ ->
                    if compare p p1 /= compare p1 p2 then
                        result ++ List.reverse list
                    else
                        sortSplitPoints ( p :: intersections, I p :: result ) rest

                _ ->
                    sortSplitPoints ( p :: intersections, I p :: result ) rest



-- (3) Construct polygons


fromSplitPoints : ( Polygon, Polygon, List Polygon ) -> List SplitPoint -> List Polygon
fromSplitPoints ( working, waiting, completed ) list =
    case list of
        [] ->
            working :: completed

        (P p) :: rest ->
            fromSplitPoints ( p :: working, waiting, completed ) rest

        (I p) :: rest ->
            if waiting |> List.isEmpty then
                -- start new polygon
                fromSplitPoints ( p :: waiting, p :: working, completed ) rest
            else
                -- end current polygon
                fromSplitPoints ( p :: waiting, [], (p :: working) :: completed ) rest



--


(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f g x y =
    g (f x y)
