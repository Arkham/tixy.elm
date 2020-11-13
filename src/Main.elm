module Main exposing (main)

import Html exposing (Html)
import Html.Attributes


main : Html a
main =
    Html.section []
        [ Html.h1 [] [ Html.text "Minimal Elm + Parcel implementation, deployed to Netlify" ]
        , Html.p []
            [ Html.text "Find the source on "
            , Html.a [ Html.Attributes.href "https://github.com/JoelQ/elm-netlify-parcel" ] [ Html.text "GitHub" ]
            ]
        ]
