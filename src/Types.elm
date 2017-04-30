module Types exposing (Radians, Polyline, Renderable, Moving)

import Math.Vector3 as Vector3 exposing (Vec3)


type alias Radians =
    Float


type alias Polyline =
    List Vec3


type alias Renderable a =
    { a
        | polylines : List Polyline
        , position : Vec3
        , rotation : Radians
    }


type alias Moving a =
    { a
        | velocity : Vec3
        , rotationInertia : Radians
    }
