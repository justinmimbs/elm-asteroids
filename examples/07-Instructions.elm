module Main exposing (main)

import Html exposing (Html)
import Html.Attributes


-- project

import Screen
import Static


main : Html a
main =
    Html.main_
        []
        [ Html.node "style"
            []
            [ Html.text "@import url(../app/style.css);"
            ]
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
