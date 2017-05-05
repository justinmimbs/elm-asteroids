module AsteroidShapes exposing (main)

import AnimationFrame
import Html exposing (Html)
import Math.Vector3 as Vector3 exposing (Vec3)
import Random.Pcg as Random
import Time exposing (Time)


-- project modules

import Asteroid exposing (Asteroid)
import Force
import Main exposing (view, wrapPosition)


main : Program Never (List Asteroid) Time
main =
    Html.program
        { init = ( initField, Cmd.none )
        , update = \x r -> ( update x r, Cmd.none )
        , view = view
        , subscriptions = always (AnimationFrame.diffs Time.inSeconds)
        }


update : Time -> List Asteroid -> List Asteroid
update dt =
    List.map
        (\obj ->
            { obj
                | position = obj.velocity |> Vector3.scale dt |> Vector3.add obj.position
                , rotation = obj.rotationInertia * dt + obj.rotation
            }
                |> wrapPosition
        )


initField : List Asteroid
initField =
    Random.initialSeed 3780540833
        |> Random.step (Asteroid.field 200 ( 1200, 900 ) 10)
        |> Tuple.first
        |> Force.separate
