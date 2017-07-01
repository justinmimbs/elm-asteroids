module Types exposing (Radians, Polyline, Positioned, Moving, Expiring)

import Time exposing (Time)


-- project

import Geometry.Vector exposing (Vector, Point)


type alias Radians =
    Float


type alias Polyline =
    List Point


type alias Positioned a =
    { a
        | position : Point
        , rotation : Radians
    }


type alias Moving a =
    { a
        | velocity : Vector
        , angularVelocity : Radians
    }


type alias Expiring a =
    { a
        | timeRemaining : Time
    }
