module Geometry.Force exposing (separate)

import Dict exposing (Dict)


-- project modules

import Geometry.Vector as Vector exposing (Point)


type alias Circle a =
    { a
        | position : Point
        , radius : Float
    }


separate : List (Circle a) -> List (Circle a)
separate nodes =
    let
        dict0 =
            nodes |> List.indexedMap (,) |> Dict.fromList

        pairs =
            combinations2 (,) (Dict.keys dict0)

        iterations =
            8
    in
        List.range 1 iterations
            |> List.foldl
                (\i -> separateIteration (toFloat i / iterations |> sqrt) pairs)
                dict0
            |> Dict.values


separateIteration : Float -> List ( Int, Int ) -> Dict Int (Circle a) -> Dict Int (Circle a)
separateIteration stepCoeff pairs dict0 =
    pairs
        |> List.foldl
            (\( ia, ib ) dict ->
                Maybe.map2
                    (separate2 stepCoeff)
                    (Dict.get ia dict)
                    (Dict.get ib dict)
                    |> Maybe.andThen identity
                    |> Maybe.map
                        (\( a, b ) ->
                            dict
                                |> Dict.insert ia a
                                |> Dict.insert ib b
                        )
                    |> Maybe.withDefault
                        dict
            )
            dict0


separate2 : Float -> Circle a -> Circle a -> Maybe ( Circle a, Circle a )
separate2 stepCoeff a b =
    let
        distSq =
            Vector.distanceSquared a.position b.position

        distSqMin =
            (a.radius + b.radius) ^ 2
    in
        if distSq >= distSqMin then
            Nothing
        else
            let
                distOverlap =
                    (a.radius + b.radius) - sqrt distSq

                dir =
                    if distSq == 0 then
                        Vector.normalize ( 1, 1 )
                    else
                        Vector.direction b.position a.position

                v =
                    Vector.scale ((distOverlap / 2) * stepCoeff) dir
            in
                Just
                    ( { a | position = Vector.add a.position v }
                    , { b | position = Vector.sub b.position v }
                    )


{-| List all k-combinations where k = 2
-}
combinations2 : (a -> a -> b) -> List a -> List b
combinations2 f list =
    list
        |> List.indexedMap
            (\i a -> list |> List.drop (i + 1) |> List.map (f a))
        |> List.concat
