module Types exposing (Radians, Polyline, Positioned, Moving, Expiring, Renderable)

import Math.Vector3 as Vector3 exposing (Vec3)
import Time exposing (Time)


type alias Radians =
    Float


type alias Polyline =
    List Vec3


type alias Positioned a =
    { a
        | position : Vec3
        , rotation : Radians
    }


type alias Moving a =
    { a
        | velocity : Vec3
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
