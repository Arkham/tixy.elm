module Main exposing (main)

import Browser
import Browser.Events as Events
import Expression exposing (Expression)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events
import List.Zipper as Zipper exposing (Zipper)
import Parser
import Time
import Tutorial exposing (Tutorial)


type alias Model =
    { time : Float
    , input : String
    , expression : Maybe Expression
    , help : String
    , tutorials : Zipper Tutorial
    }


initialModel : Model
initialModel =
    let
        allTutorials =
            Tutorial.all

        { description, source } =
            Zipper.current allTutorials
    in
    { time = 0
    , input = source
    , expression = parse source
    , help = description
    , tutorials = allTutorials
    }


parse : String -> Maybe Expression
parse str =
    case Parser.run Expression.parser str of
        Ok value ->
            Just value

        Err err ->
            Nothing


type Msg
    = Tick Float
    | ChangeInput String
    | NextTutorial


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt }, Cmd.none )

        NextTutorial ->
            let
                newTutorials =
                    Zipper.next model.tutorials
                        |> Maybe.withDefault model.tutorials

                { description, source } =
                    Zipper.current newTutorials
            in
            ( { model
                | time = 0
                , tutorials = newTutorials
                , help = description
                , input = source
                , expression = parse source
              }
            , Cmd.none
            )

        ChangeInput str ->
            let
                newExpression =
                    case ( String.trim str, parse str ) of
                        ( "", _ ) ->
                            Nothing

                        ( _, Just v ) ->
                            Just v

                        ( _, Nothing ) ->
                            model.expression

                newTime =
                    case newExpression of
                        Just _ ->
                            0

                        Nothing ->
                            model.time
            in
            ( { model
                | input = str
                , expression = newExpression
                , time = newTime
              }
            , Cmd.none
            )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Events.onAnimationFrameDelta Tick
        }


styleFromValue : Float -> String
styleFromValue value =
    String.concat
        [ "transform: scale("
        , String.fromFloat value
        , "); background-color: "
        , if value < 0 then
            "#f24"

          else
            "white"
        , ";"
        ]


callTixyFn : Maybe Expression -> Float -> Int -> Float
callTixyFn exp t intI =
    let
        i =
            toFloat intI

        x =
            modBy 16 intI

        y =
            intI // 16

        tixy =
            { t = t / 1000
            , i = i
            , x = toFloat x
            , y = toFloat y
            }

        result =
            exp
                |> Maybe.map (Expression.evaluate tixy)
                |> Maybe.withDefault 0
    in
    clamp -1 1 result


viewField : Maybe Expression -> Float -> Int -> Html msg
viewField exp t i =
    div [ class "field", attribute "style" (styleFromValue (callTixyFn exp t i)) ] []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.h1 [] [ text "tixy.elm" ]
        , div [ class "map", Html.Events.onClick NextTutorial ] <|
            (List.range 0 255 |> List.map (viewField model.expression model.time))
        , div [ class "editor" ]
            [ viewHelp model.help
            , Html.span [] [ text "(t,i,x,y) =>" ]
            , Html.input
                [ Attr.value model.input
                , Html.Events.onInput ChangeInput
                , Attr.attribute "autocomplete" "off"
                , Attr.attribute "autocapitalize" "off"
                , Attr.attribute "spellcheck" "false"
                , Attr.attribute "enterkeyhint" "go"
                ]
                []
            ]
        ]


viewHelp : String -> Html msg
viewHelp str =
    div [ class "help" ]
        (str
            |> String.split "\n"
            |> List.map
                (\line ->
                    div [ class "help-line" ] [ text ("// " ++ line) ]
                )
        )
