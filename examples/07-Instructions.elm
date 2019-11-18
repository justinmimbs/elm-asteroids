module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Screen
import Static


main : Html a
main =
    Html.main_
        []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "../app/style.css" ] []
        , Html.div
            [ Html.Attributes.class "screen-container"
            ]
            [ [] |> Screen.render ( 1200, 900 )
            ]
        , Html.div
            [ Html.Attributes.class "instructions-container"
            ]
            [ Static.instructions
            ]
        ]
