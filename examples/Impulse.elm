module Impulse exposing (main)

import AnimationFrame
import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time exposing (Time)


-- project

import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Main exposing (transformPoints, wrapPosition, updateMoving, updateExpiring)
import Screen
import Types exposing (Moving, Positioned, Radians)


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
                    let
                        ( vel, ang ) =
                            impulse (Vector.sub p0 p1) p0 disk.position

                        diskNext =
                            { disk
                                | velocity = disk.velocity |> Vector.add vel
                                , angularVelocity = disk.angularVelocity + ang
                            }
                    in
                        { model | drag = Nothing, disk = diskNext }

                Nothing ->
                    model

        Tick dt ->
            { model | disk = model.disk |> updateMoving dt |> wrapPosition }


impulse : Vector -> Point -> Point -> ( Vector, Radians )
impulse velocity contact center =
    let
        direction =
            Vector.direction contact center

        speed =
            Vector.length velocity

        circum =
            2 * pi * (Vector.distance center contact)

        angle =
            angleFrom (Vector.normalize velocity) direction

        angSpeed =
            (speed / circum) * 2 * pi * signum angle

        -- rotation alpha
        t =
            abs angle / (pi / 2)
    in
        ( direction |> Vector.scale (speed * (1 - t))
        , angSpeed * t
        )


{-| Directed angle; assumes unit vectors.
-}
angleFrom : Vector -> Vector -> Float
angleFrom a b =
    atan2 (Vector.cross a b) (Vector.dot a b)


signum : Float -> Float
signum x =
    if x > 0 then
        1
    else if x == 0 then
        0
    else
        -1



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

        paths : List ( Bool, List Point )
        paths =
            [ disk |> transformPolygon |> (,) True ]
    in
        Svg.svg
            (attributes ++ events)
            [ drag |> Maybe.map viewLine |> Maybe.withDefault (Svg.g [] [])
            , Screen.render screenSize paths
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
