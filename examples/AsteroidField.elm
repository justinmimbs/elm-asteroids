module AsteroidShapes exposing (main)

import AnimationFrame
import Html exposing (Html)
import Random.Pcg as Random
import Time exposing (Time)


-- project modules

import Asteroid exposing (Asteroid)
import Geometry.Vector as Vector
import Util exposing (transformPoints, wrapPosition)
import Screen
import Types exposing (Moving, Positioned)


main : Program Never (List Asteroid) Time
main =
    Html.program
        { init = ( initField, Cmd.none )
        , update = \x r -> ( update x r, Cmd.none )
        , view = List.map asteroidToPath >> Screen.render screenSize
        , subscriptions = always (AnimationFrame.diffs Time.inSeconds)
        }


update : Time -> List Asteroid -> List Asteroid
update dt =
    List.map
        (updateMoving dt >> wrapPosition screenSize)


updateMoving : Time -> Moving (Positioned a) -> Moving (Positioned a)
updateMoving dt obj =
    { obj
        | position = obj.position |> Vector.add (obj.velocity |> Vector.scale dt)
        , rotation = obj.rotation + obj.angularVelocity * dt
    }


initField : List Asteroid
initField =
    Random.initialSeed 3780540833
        |> Random.step (Asteroid.field ( 1200, 900 ) 200 10)
        |> Tuple.first


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    ( 1, True, polygon |> transformPoints position rotation )
