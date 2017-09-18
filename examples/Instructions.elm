module Instructions exposing (main)

import Html exposing (Html)
import Html.Attributes


-- project

import Screen
import Static


main : Html a
main =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ [] |> Screen.render ( 1200, 900 )
        , Static.instructions
        ]
