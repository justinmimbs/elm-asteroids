module Geometry.Circle exposing (Circle, enclose)

import Geometry.Line as Line exposing (Intersection(LineLine), midpoint)
import Geometry.Vector exposing (Point, distanceSquared)


circumcenter3 : Point -> Point -> Point -> Maybe Point
circumcenter3 a b c =
    let
        ( p1, p2 ) =
            Line.perpendicularBisector a b

        ( p3, p4 ) =
            Line.perpendicularBisector a c
    in
        Line.intersect LineLine p1 p2 p3 p4



-- smallest enclosing circle


{-| ( center, radius )
-}
type alias Circle =
    ( Point, Float )


{-| ( center, radiusSquared )
-}
type alias Circ =
    ( Point, Float )


toCircle : Circ -> Circle
toCircle ( c, rs ) =
    ( c, sqrt rs )


isEnclosed : Circ -> Point -> Bool
isEnclosed ( c, rs ) p =
    -- `distanceSquared c p <= rs` is the ideal comparison
    distanceSquared c p - rs <= epsilon * rs * 10


epsilon : Float
epsilon =
    -- machine epsilon for double-precision float
    2 ^ -52


circumcirc2 : Point -> Point -> Circ
circumcirc2 a b =
    let
        center =
            midpoint a b
    in
        ( center, distanceSquared a center )


circumcirc3 : Point -> Point -> Point -> Maybe Circ
circumcirc3 a b c =
    circumcenter3 a b c
        |> Maybe.map
            (\center ->
                ( center, distanceSquared a center )
            )


enclose3 : Point -> Point -> Point -> Circ
enclose3 a b c =
    let
        ab =
            circumcirc2 a b

        ac =
            circumcirc2 a c

        bc =
            circumcirc2 b c
    in
        if isEnclosed ab c then
            ab
        else if isEnclosed ac b then
            ac
        else if isEnclosed bc a then
            bc
        else
            circumcirc3 a b c |> Maybe.withDefault ( a, 0 )



-- minidisk


{-| Compute the smallest enclosing circle from a list of points in linear time.
Based on [Emo Welzl's algorithm](https://www.inf.ethz.ch/personal/emo/PublFiles/SmallEnclDisk_LNCS555_91.pdf).
This version assumes the list is already in a random order.
-}
enclose : List Point -> Circle
enclose points =
    case points of
        [] ->
            ( ( 0, 0 ), 0 )

        [ a ] ->
            ( a, 0 )

        [ a, b ] ->
            circumcirc2 a b
                |> toCircle

        a :: b :: c :: rest ->
            encloseHelp rest a b c [] (enclose3 a b c)
                |> toCircle


encloseHelp : List Point -> Point -> Point -> Point -> List Point -> Circ -> Circ
encloseHelp points a b c enclosed circ =
    case points of
        [] ->
            circ

        p :: rest ->
            if p |> isEnclosed circ then
                encloseHelp rest a b c (p :: enclosed) circ
            else
                encloseHelp (c :: enclosed ++ rest) p a b [] (enclose3 p a b)
