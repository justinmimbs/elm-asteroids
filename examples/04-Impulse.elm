module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes
import Json.Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time exposing (Time)


-- project

import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Physics exposing (Movement)
import Screen
import Types exposing (Moving, Positioned, Radians)
import Util exposing (transformPoints, wrapPosition)


type alias Model =
    { drag : Maybe ( Point, Point )
    , disk : Disk
    }


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { drag = Nothing
              , disk = toDisk 50 12 ( 600, 450 ) 0 Vector.zero 0
              }
            , Cmd.none
            )
        , update = \x r -> ( update x r, Cmd.none )
        , view = view
        , subscriptions = always (AnimationFrame.diffs (Time.inSeconds >> Tick))
        }


type alias Disk =
    Moving (Positioned { radius : Float, polygon : Polygon })


toDisk : Float -> Int -> Point -> Radians -> Vector -> Radians -> Disk
toDisk radius sides p r v a =
    { radius = radius
    , polygon = Polygon.ngon sides |> List.map (Vector.scale radius)
    , position = p
    , rotation = r
    , velocity = v
    , angularVelocity = a
    }



-- update


type Msg
    = MouseDown Point
    | MouseMove Point
    | MouseUp
    | Tick Time


update : Msg -> Model -> Model
update msg ({ drag, disk } as model) =
    case msg of
        MouseDown p0 ->
            { model | drag = Just ( p0, p0 ) }

        MouseMove p1 ->
            case drag of
                Just ( p0, _ ) ->
                    { model | drag = Just ( p0, p1 ) }

                Nothing ->
                    model

        MouseUp ->
            case drag of
                Just ( p0, p1 ) ->
                    { model
                        | drag = Nothing
                        , disk = disk |> addMovement (Physics.impulse (Vector.sub p1 p0) p1 disk.position)
                    }

                Nothing ->
                    model

        Tick dt ->
            { model | disk = model.disk |> updateMoving dt |> wrapPosition screenSize }


updateMoving : Time -> Moving (Positioned a) -> Moving (Positioned a)
updateMoving dt obj =
    { obj
        | position = obj.position |> Vector.add (obj.velocity |> Vector.scale dt)
        , rotation = obj.rotation + obj.angularVelocity * dt
    }


addMovement : Movement -> Moving a -> Moving a
addMovement ( v, av ) a =
    { a | velocity = a.velocity |> Vector.add v, angularVelocity = a.angularVelocity + av }



-- view


screenSize : ( Float, Float )
screenSize =
    ( 1200, 900 )


( width, height ) =
    screenSize


view : Model -> Html Msg
view { drag, disk } =
    let
        attributes =
            [ Svg.Attributes.width (width |> px)
            , Svg.Attributes.height (height |> px)
            , Svg.Attributes.style "cursor: default;"
            ]

        events =
            drag
                |> Maybe.map
                    (always [ mousemove, mouseup ])
                |> Maybe.withDefault [ mousedown ]

        paths : List Screen.Path
        paths =
            [ disk |> transformPolygon |> (,,) 1 True ]
    in
        Html.div
            [ Html.Attributes.style
                [ ( "height", "100vh" )
                , ( "fill", "none" )
                , ( "stroke", "gray" )
                , ( "stroke-width", "2px" )
                ]
            ]
            [ Svg.svg
                (attributes ++ events)
                [ drag |> Maybe.map viewLine |> Maybe.withDefault (Svg.g [] [])
                , paths |> Screen.render screenSize
                ]
            ]


viewLine : ( Point, Point ) -> Svg a
viewLine ( ( x1, y1 ), ( x2, y2 ) ) =
    Svg.line
        [ Svg.Attributes.x1 (x1 |> px)
        , Svg.Attributes.y1 (y1 |> px)
        , Svg.Attributes.x2 (x2 |> px)
        , Svg.Attributes.y2 (y2 |> px)
        , Svg.Attributes.style "stroke: rgba(0, 0, 0, 0.1)"
        ]
        []


px : Float -> String
px =
    toString >> (++) >> (|>) "px"


transformPolygon : Positioned { a | polygon : Polygon } -> Polygon
transformPolygon { polygon, position, rotation } =
    polygon |> transformPoints position rotation



-- events


mouseup : Svg.Attribute Msg
mouseup =
    Svg.Events.on "mouseup" (Json.Decode.succeed MouseUp)


mousedown : Svg.Attribute Msg
mousedown =
    Svg.Events.on "mousedown" (decodeMouseOffset |> Json.Decode.map MouseDown)


mousemove : Svg.Attribute Msg
mousemove =
    Svg.Events.on "mousemove" (decodeMouseOffset |> Json.Decode.map MouseMove)


decodeMouseOffset : Decoder Point
decodeMouseOffset =
    Json.Decode.map2
        (,)
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)
