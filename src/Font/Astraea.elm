module Font.Astraea exposing (pathData)

import Dict exposing (Dict)


-- project

import Font exposing (Font)
import PathData exposing (PathData, Command(M, L, C))


pathData : Font PathData
pathData =
    { width = 32
    , height = 48
    , replacement = [ M 16 10, L 30 24, L 16 38, L 2 24, L 16 10 ]
    , characters =
        [ '1' => [ M 10 6, L 20 0, L 20 48 ]
        , '2' => [ M 5 8, C 6 4 10 0 16 0, C 24 0 28 6 28 12, C 28 18 25 21 18 30, L 4 48, L 30 48 ]
        , '3' => [ M 5 0, L 27 0, L 11 19, C 13 18 13 18 16 18, C 24 18 30 25 30 33, C 30 41 24 48 16 48, C 10 48 5 44 3 38 ]
        , '4' => [ M 30 34, L 2 34, L 24 0, L 24 48 ]
        , '5' => [ M 27 0, L 7 0, L 4 22, C 10 18 10 18 16 18, C 24 18 30 25 30 33, C 30 41 24 48 16 48, C 10 48 5 44 3 38 ]
        , '6' => [ M 20 0, L 5 24, C 3 27 2 30 2 33, C 2 41 8 48 16 48, C 24 48 30 41 30 33, C 30 25 24 18 16 18, C 11 18 8 20 5 24 ]
        , '7' => [ M 3 0, L 29 0, L 11 48 ]
        ]
            |> Dict.fromList
    }


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
