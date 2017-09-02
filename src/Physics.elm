module Physics exposing (Movement, impulse, Collidable, collide)

import Geometry.Matrix as Matrix
import Geometry.Polygon as Polygon exposing (Polygon)
import Geometry.Vector as Vector exposing (Vector, Point)


type alias Radians =
    Float


{-| ( velocity, angularVelocity )
-}
type alias Movement =
    ( Vector, Radians )


impulse : Vector -> Point -> Point -> Movement
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


type alias Collidable a =
    { a
        | radius : Float
        , polygon : Polygon
        , position : Point
        , rotation : Radians
        , velocity : Vector
        , angularVelocity : Radians
    }


collide : Float -> Collidable a -> Collidable b -> Maybe ( Movement, Movement )
collide e a b =
    contactPoint a b |> Maybe.andThen (collideAtPoint e a b)


contactPoint : Collidable a -> Collidable b -> Maybe Point
contactPoint a b =
    if Vector.distance a.position b.position < a.radius + b.radius then
        Polygon.intersectionsWithPolygon
            (transformPolygon a)
            (transformPolygon b)
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

        [] ->
            Nothing


collideAtPoint : Float -> Collidable a -> Collidable b -> Point -> Maybe ( Movement, Movement )
collideAtPoint e a b contact =
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
                ( aMass, bMass ) =
                    ( a.radius ^ 2, b.radius ^ 2 )

                t =
                    aMass / (aMass + bMass)

                aReflect =
                    a.velocity |> Vector.reflect (Vector.direction b.position a.position)

                bReflect =
                    b.velocity |> Vector.reflect (Vector.direction a.position b.position)

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
                    ( ( a.velocity
                            |> Vector.interpolate (0 + t) aReflect
                            |> Vector.add (aPush |> Vector.scale ((1 - t) * 2))
                            |> Vector.interpolate e inelasticVel
                      , a.angularVelocity
                            |> interpolate (0 + t) aSpin
                            |> interpolate e inelasticAngVel
                      )
                    , ( b.velocity
                            |> Vector.interpolate (1 - t) bReflect
                            |> Vector.add (bPush |> Vector.scale ((0 + t) * 2))
                            |> Vector.interpolate e inelasticVel
                      , b.angularVelocity
                            |> interpolate (1 - t) bSpin
                            |> interpolate e inelasticAngVel
                      )
                    )
        else
            Nothing


transformPolygon : Collidable a -> Polygon
transformPolygon { polygon, position, rotation } =
    List.map
        (Matrix.transform (Matrix.init 1 rotation position))
        polygon


angleBetween : Vector -> Vector -> Float
angleBetween a b =
    acos (Vector.dot (Vector.normalize a) (Vector.normalize b))


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
