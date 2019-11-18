module Main exposing (main)

import Asteroid exposing (Asteroid)
import Geometry.Vector as Vector
import Html exposing (Html)
import Html.Attributes
import Random
import Screen
import Util exposing (transformPoints)


main : Html a
main =
    Random.initialSeed 3780540833
        |> Random.step (Random.list (7 * 5) Asteroid.asteroid)
        |> Tuple.first
        |> List.map2
            (\pos object -> { object | position = pos } |> asteroidToPath)
            (gridPositions ( 7, 5 ) ( 150, 150 ) |> List.map (Vector.add ( 150, 150 )))
        |> Screen.render ( 1200, 900 )
        |> List.singleton
        |> Html.div
            [ Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "fill" "none"
            , Html.Attributes.style "stroke" "gray"
            , Html.Attributes.style "stroke-width" "2px"
            ]


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    Screen.Path 1 True (polygon |> transformPoints position rotation)


gridPositions : ( Int, Int ) -> ( Float, Float ) -> List ( Float, Float )
gridPositions ( columns, rows ) ( width, height ) =
    cross
        (\i j -> ( toFloat j * width, toFloat i * height ))
        (List.range 0 (rows - 1))
        (List.range 0 (columns - 1))


cross : (a -> b -> c) -> List a -> List b -> List c
cross f xs ys =
    List.concatMap (\x -> List.map (f x) ys) xs
