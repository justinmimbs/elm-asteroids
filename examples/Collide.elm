module Collide exposing (..)

import AnimationFrame
import Html exposing (Html)
import Mouse
import Time exposing (Time)


-- project modules

import Geometry.Force as Force
import Geometry.Line as Line exposing (Intersection(SegmentSegment))
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)
import Main exposing (viewPaths, transformPoints, wrapPosition, updateMoving)
import Screen
import Types exposing (Moving, Positioned, Radians)


init : List (List Disk)
init =
    let
        pairs =
            [ [ toDisk ( 300, 100 ) ( 300, 300 ) (pi / 3)
              , toDisk ( 600, 400 ) ( 0, 0 ) 0
              ]
            , [ toDisk ( 100, 100 ) ( 300, 300 ) (pi / 3)
              , toDisk ( 300, 300 ) ( 200, 200 ) 0
              ]
            , [ toDisk ( 300, 100 ) ( 300, 330 ) 0
              , toDisk ( 600, 400 ) ( 0, 0 ) 0
              ]
            , [ toDisk ( 300, 100 ) ( 300, 430 ) (pi / 3)
              , toDisk ( 600, 400 ) ( 0, 0 ) 0
              ]
            , [ toDisk ( 300, 100 ) ( 300, 300 ) (pi / 3)
              , toDisk ( 400, 600 ) ( 200, -200 ) 0
              ]
            , [ toDisk ( 300, 100 ) ( 350, 50 ) (pi / 8)
              , toDisk ( 900, 100 ) ( -150, 70 ) (pi / -2)
              ]
            , [ toDisk ( 300, 100 ) ( 250, 100 ) (pi / 3)
              , toDisk ( 900, 100 ) ( -250, 100 ) (pi / -3)
              ]
            , [ toDisk ( 300, 100 ) ( 350, 270 ) (pi / -5)
              , toDisk ( 900, 700 ) ( -150, -150 ) (pi / 2)
              ]
            ]
    in
        List.map (toPair 50 50) pairs
            ++ List.map (toPair 30 50) pairs
            ++ List.map (toPair 50 30) pairs


toPair : Float -> Float -> List (Float -> Disk) -> List Disk
toPair a b pair =
    case pair of
        [ f, g ] ->
            [ f a, g b ]

        _ ->
            []


main : Program Never (List (List Disk)) Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = \x r -> ( update x r, Cmd.none )
        , view =
            List.head
                >> Maybe.map (List.map (transformPolygon >> (,) True) >> viewPaths)
                >> Maybe.withDefault (Html.text "")
        , subscriptions =
            Sub.batch
                [ AnimationFrame.diffs (Tick << Time.inSeconds)
                , Mouse.downs (always Next)
                ]
                |> always
        }


type Msg
    = Tick Time
    | Next



--


type alias Disk =
    Moving (Positioned { radius : Float, polygon : Polygon })


toDisk : Point -> Vector -> Radians -> Float -> Disk
toDisk p v a radius =
    { radius = radius
    , polygon = Polygon.ngon 12 |> List.map (Vector.scale radius)
    , position = p
    , rotation = 0
    , velocity = v
    , angularVelocity = a
    }



--


update : Msg -> List (List Disk) -> List (List Disk)
update msg lists =
    case ( msg, lists ) of
        ( Tick dt, disks :: rest ) ->
            updateDisks dt disks :: rest

        ( Next, _ :: rest ) ->
            rest

        _ ->
            lists


updateDisks : Time -> List Disk -> List Disk
updateDisks dt disks =
    case disks |> List.map (updateMoving dt >> wrapPosition) of
        [ a, b ] ->
            case collide 0.9 (a.radius ^ 2) (b.radius ^ 2) a b of
                Just ( a2, b2 ) ->
                    [ a2, b2 ]

                Nothing ->
                    [ a, b ]

        x ->
            x



--


type alias Collidable a =
    Positioned (Moving { a | polygon : Polygon, radius : Float })


collide : Float -> Float -> Float -> Collidable a -> Collidable b -> Maybe ( Collidable a, Collidable b )
collide e aMass bMass a b =
    contactPoint a b |> Maybe.map (Debug.log "contact") |> Maybe.andThen (collideAtPoint e aMass bMass a b)


contactPoint : Collidable a -> Collidable b -> Maybe Point
contactPoint a b =
    if Vector.distance a.position b.position < a.radius + b.radius then
        intersectionsPolygonPolygon
            (transformPolygon b)
            (transformPolygon a)
            |> meanPoint
    else
        Nothing


meanPoint : List Point -> Maybe Point
meanPoint points =
    case points of
        head :: tail ->
            tail
                |> List.foldl (\x ( r, n ) -> ( Vector.add x r, n + 1 )) ( head, 1 )
                |> (\( r, n ) -> Vector.scale (1 / n) r)
                |> Just

        _ ->
            Nothing


collideAtPoint : Float -> Float -> Float -> Collidable a -> Collidable b -> Point -> Maybe ( Collidable a, Collidable b )
collideAtPoint e aMass bMass a b contact =
    let
        aSpeed =
            a.velocity |> Vector.length

        bSpeed =
            b.velocity |> Vector.length

        aToward =
            angleBetween a.velocity (Vector.sub contact a.position) < pi / 2

        bToward =
            angleBetween b.velocity (Vector.sub contact b.position) < pi / 2
    in
        if aToward && bToward || aToward && aSpeed > bSpeed || bToward && bSpeed > aSpeed then
            let
                t =
                    aMass / (aMass + bMass)

                aReflect =
                    a.velocity |> reflect (Vector.direction b.position a.position)

                bReflect =
                    b.velocity |> reflect (Vector.direction a.position b.position)

                ( aPush, aSpin ) =
                    a.position |> impulse b.velocity contact

                ( bPush, bSpin ) =
                    b.position |> impulse a.velocity contact

                inelasticVel =
                    Vector.interpolate t b.velocity a.velocity

                inelasticAngVel =
                    interpolate t b.angularVelocity a.angularVelocity
            in
                Just
                    ( { a
                        | velocity =
                            Vector.interpolate e
                                inelasticVel
                                (a.velocity |> Vector.interpolate (0 + t) aReflect |> Vector.add (aPush |> Vector.scale ((1 - t) * 2)))
                        , angularVelocity = interpolate e inelasticAngVel (a.angularVelocity |> interpolate (0 + t) aSpin)
                      }
                    , { b
                        | velocity =
                            Vector.interpolate e
                                inelasticVel
                                (b.velocity |> Vector.interpolate (1 - t) bReflect |> Vector.add (bPush |> Vector.scale ((0 + t) * 2)))
                        , angularVelocity = interpolate e inelasticAngVel (b.angularVelocity |> interpolate (1 - t) bSpin)
                      }
                    )
        else
            Nothing


reflect : Vector -> Vector -> Vector
reflect normal vector =
    Vector.sub vector (normal |> Vector.scale (2 * Vector.dot normal vector))


impulse : Vector -> Point -> Point -> ( Vector, Radians )
impulse velocity contact center =
    let
        direction =
            Vector.direction contact center

        speed =
            Vector.length velocity

        angle =
            angleFrom (Vector.normalize velocity) direction

        angSpeed =
            (speed / (Vector.distance center contact)) * signum angle

        -- rotation alpha
        t =
            abs angle / (pi / 2)
    in
        ( direction |> Vector.scale (speed * (1 - t))
        , angSpeed
            * (if t > 1 then
                2 - t
               else
                t
              )
        )


on : (a -> b) -> (b -> b -> c) -> a -> a -> c
on f g x y =
    g (f x) (f y)


angleBetween : Vector -> Vector -> Float
angleBetween =
    angleBetweenUnit |> on Vector.normalize


{-| Assumes unit vectors.
-}
angleBetweenUnit : Vector -> Vector -> Float
angleBetweenUnit a b =
    acos (Vector.dot a b)


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


interpolate : Float -> Float -> Float -> Float
interpolate t a b =
    (a * (1 - t)) + (b * t)



-- duplicated


intersectionsPolygonSegment : Polygon -> Point -> Point -> List Point
intersectionsPolygonSegment polygon a b =
    Polygon.fold
        (Line.intersect SegmentSegment a b >>> unwrap identity (::))
        []
        polygon


intersectionsPolygonPolygon : Polygon -> Polygon -> List Point
intersectionsPolygonPolygon polygon =
    Polygon.fold
        (intersectionsPolygonSegment polygon >>> (++))
        []


transformPolygon : Positioned { a | polygon : Polygon } -> Polygon
transformPolygon { polygon, position, rotation } =
    polygon |> transformPoints position rotation


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f m =
    case m of
        Just x ->
            f x

        Nothing ->
            default


(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f g x y =
    g (f x y)
