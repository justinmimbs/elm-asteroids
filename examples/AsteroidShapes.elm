module AsteroidShapes exposing (main)

import Html exposing (Html)
import Random.Pcg as Random


-- project modules

import Asteroid exposing (Asteroid)
import Geometry.Vector as Vector
import Main exposing (viewPaths, transformPoints)
import Screen


main : Html a
main =
    Random.initialSeed 3780540833
        |> Random.step (Random.list (7 * 5) Asteroid.asteroid)
        |> Tuple.first
        |> List.map2
            (\pos object -> { object | position = pos } |> asteroidToPath)
            (gridPositions ( 7, 5 ) ( 150, 150 ) |> List.map (Vector.add ( 150, 150 )))
        |> viewPaths


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    ( True
    , polygon |> transformPoints position rotation
    )


gridPositions : ( Int, Int ) -> ( Float, Float ) -> List ( Float, Float )
gridPositions ( columns, rows ) ( width, height ) =
    List.map
        (\i j -> ( toFloat j * width, toFloat i * height ))
        (List.range 0 (rows - 1))
        |> apply (List.range 0 (columns - 1))


{-| <*> for non-determinism List Applicative
-}
apply : List a -> List (a -> b) -> List b
apply =
    List.concatMap << (flip List.map)
