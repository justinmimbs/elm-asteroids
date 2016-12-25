module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import Screen


type alias Polyline =
    List Vec3


type alias Radians =
    Float


type alias Object =
    { polylines : List Polyline
    , scale : Float
    , rotation : Radians
    , position : Vec3
    }


spaceship : List Polyline
spaceship =
    [ [ ( 0.5, -1 )
      , ( 0, 1 )
      , ( -0.5, -1 )
      , ( 0.5, -1 )
      ]
    , [ ( 0.5, -1 )
      , ( 1, -0.5 )
      , ( 0, 0 )
      , ( -1, -0.5 )
      , ( -0.5, -1 )
      ]
    ]
        |> List.map (List.map toVec3)


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


globalTransform : Mat4
globalTransform =
    Matrix4.identity
        |> Matrix4.translate3
            (Tuple.first screenSize / 2)
            (Tuple.second screenSize / 2)
            0
        |> Matrix4.scale3 1 -1 1


main : Html a
main =
    view
        [ { polylines = spaceship
          , scale = 20
          , rotation = 0
          , position = Vector3.vec3 0 0 0
          }
        ]


toVec3 : ( Float, Float ) -> Vec3
toVec3 ( x, y ) =
    Vector3.vec3 x y 0


view : List Object -> Html a
view objects =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            ]
        ]
        [ objects
            |> List.concatMap (transformObject globalTransform)
            |> Screen.render screenSize
        ]


transformObject : Mat4 -> Object -> List Polyline
transformObject parentTransform object =
    let
        transform =
            Matrix4.identity
                |> Matrix4.translate object.position
                |> Matrix4.rotate object.rotation Vector3.k
                |> Matrix4.scale3 object.scale object.scale object.scale
                |> Matrix4.mul parentTransform
    in
        List.map
            (List.map (Matrix4.transform transform))
            object.polylines
