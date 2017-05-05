module Force exposing (Circle, separate)

import Dict exposing (Dict)
import Math.Vector3 as Vector3 exposing (Vec3)


type alias Circle a =
    { a
        | position : Vec3
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
            Vector3.distanceSquared a.position b.position

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
                        Vector3.normalize (Vector3.vec3 1 1 0)
                    else
                        Vector3.direction a.position b.position

                v =
                    Vector3.scale ((distOverlap / 2) * stepCoeff) dir
            in
                Just
                    ( { a | position = Vector3.add a.position v }
                    , { b | position = Vector3.sub b.position v }
                    )


{-| List all k-combinations where k = 2
-}
combinations2 : (a -> a -> b) -> List a -> List b
combinations2 f list =
    list
        |> List.indexedMap
            (\i a -> list |> List.drop (i + 1) |> List.map (f a))
        |> List.concat
