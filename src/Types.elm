module Types exposing (Radians, Point, Polyline, Positioned, Moving, Expiring, Renderable)

import Time exposing (Time)


-- project

import Geometry.Vector exposing (Vector)


type alias Radians =
    Float


type alias Point =
    Vector


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
        , rotationInertia : Radians
    }


type alias Expiring a =
    { a
        | timeRemaining : Time
    }


type alias Renderable a =
    Positioned
        { a
            | polylines : List Polyline
        }
