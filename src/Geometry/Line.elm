module Geometry.Line exposing (midpoint, perpendicularBisector, Intersection(..), intersect)


type alias Point =
    ( Float, Float )


midpoint : Point -> Point -> Point
midpoint ( x1, y1 ) ( x2, y2 ) =
    ( (x1 + x2) / 2
    , (y1 + y2) / 2
    )


perpendicularBisector : Point -> Point -> ( Point, Point )
perpendicularBisector (( ax, ay ) as a) (( bx, by ) as b) =
    let
        (( mx, my ) as m) =
            midpoint a b
    in
        ( m, ( mx + (by - ay), my - (bx - ax) ) )



-- intersection


type Intersection
    = LineLine
    | LineSegment
    | SegmentSegment


intersect : Intersection -> Point -> Point -> Point -> Point -> Maybe Point
intersect intersection ( ax, ay ) ( bx, by ) ( cx, cy ) ( dx, dy ) =
    let
        ( rx, ry ) =
            ( bx - ax, by - ay )

        ( sx, sy ) =
            ( dx - cx, dy - cy )

        rs =
            -- cross r s
            rx * sy - sx * ry

        ( ex, ey ) =
            ( cx - ax, cy - ay )

        u =
            -- cross e r / rs
            (ex * ry - rx * ey) / rs
    in
        if rs /= 0 then
            case intersection of
                LineLine ->
                    Just ( cx + u * sx, cy + u * sy )

                LineSegment ->
                    if 0 <= u && u <= 1 then
                        Just ( cx + u * sx, cy + u * sy )
                    else
                        Nothing

                SegmentSegment ->
                    if 0 <= u && u <= 1 then
                        let
                            t =
                                -- cross e s / rs
                                (ex * sy - sx * ey) / rs
                        in
                            if 0 <= t && t <= 1 then
                                Just ( cx + u * sx, cy + u * sy )
                            else
                                Nothing
                    else
                        Nothing
        else
            Nothing
