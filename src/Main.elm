module Main exposing (main)

import Html exposing (Html)
import Html.Attributes


main : Html a
main =
    Html.section []
        [ Html.h1 [] [ Html.text "tixy.elm" ]
        , Html.p []
            [ Html.text "Hello there!"
            ]
        ]
