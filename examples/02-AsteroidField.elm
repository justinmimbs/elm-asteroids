module Main exposing (main)

import Asteroid exposing (Asteroid)
import Browser
import Browser.Events
import Geometry.Vector as Vector
import Html exposing (Html)
import Html.Attributes
import Random
import Screen
import Types exposing (Moving, Positioned, Time)
import Util exposing (transformPoints, wrapPosition)


main : Program () (List Asteroid) Time
main =
    Browser.element
        { init = \_ -> ( initField, Cmd.none )
        , update = \x r -> ( update x r, Cmd.none )
        , view = List.map asteroidToPath >> Screen.render screenSize >> viewContainer
        , subscriptions = always (Browser.Events.onAnimationFrameDelta (\ms -> ms / 1000))
        }


viewContainer : Html a -> Html a
viewContainer =
    let
        ( width, height ) =
            screenSize

        container =
            Html.div
                [ Html.Attributes.style "width" (String.fromFloat width ++ "px")
                , Html.Attributes.style "height" (String.fromFloat height ++ "px")
                , Html.Attributes.style "fill" "none"
                , Html.Attributes.style "stroke" "gray"
                , Html.Attributes.style "stroke-width" "2px"
                ]
    in
    \content -> container [ content ]


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
        |> Random.step (Asteroid.field screenSize 200 24)
        |> Tuple.first


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


asteroidToPath : Asteroid -> Screen.Path
asteroidToPath { polygon, position, rotation } =
    Screen.Path 1 True (polygon |> transformPoints position rotation)
