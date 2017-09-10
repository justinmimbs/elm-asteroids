module Font exposing (Font, map, typesetLine)

import Dict exposing (Dict)


type alias Font a =
    { width : Float
    , height : Float
    , replacement : a
    , characters : Dict Char a
    }


map : (a -> b) -> Font a -> Font b
map f font =
    { font
        | replacement = f font.replacement
        , characters = font.characters |> Dict.map (always f)
    }


typesetLine : (Float -> a -> b) -> Font a -> Float -> String -> List b
typesetLine typesetChar font initialOffset string =
    string
        |> String.toList
        |> List.foldl
            (\char ( offset, list ) ->
                ( offset + font.width
                , if char == ' ' then
                    list
                  else
                    font.characters
                        |> Dict.get char
                        |> Maybe.withDefault font.replacement
                        |> typesetChar offset
                        |> (flip (::)) list
                )
            )
            ( initialOffset, [] )
        |> Tuple.second
        |> List.reverse
