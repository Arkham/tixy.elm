module Main exposing (main)

import Browser
import Browser.Events as Events
import Expression exposing (Expression)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events as Events
import Parser
import Time


type alias Model =
    { time : Float
    , input : String
    , expression : Maybe Expression
    }


initialModel : Model
initialModel =
    let
        initialInput =
            "sin(t - sqrt((x-7.5)^2 + (y-6)^2))"
    in
    { time = 0
    , input = initialInput
    , expression = parse initialInput
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt }, Cmd.none )

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
            in
            ( { model
                | input = str
                , expression = newExpression
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
            "red"

          else
            "white"
        , ";"
        ]


tixyFn exp t i x y =
    exp
        |> Maybe.map (Expression.evaluate { t = t, i = i, x = x, y = y })
        |> Maybe.withDefault 0


callTixyFn exp t intI =
    let
        i =
            toFloat intI

        x =
            modBy 16 intI

        y =
            intI // 16
    in
    clamp -1 1 <| tixyFn exp (t / 1000) i (toFloat x) (toFloat y)


viewField exp t i =
    div [ class "field", attribute "style" (styleFromValue (callTixyFn exp t i)) ] []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.h1 [] [ text "tixy.elm" ]
        , div [ class "map" ] <|
            (List.range 0 255 |> List.map (viewField model.expression model.time))
        , div [ class "editor" ]
            [ Html.span [] [ text "(t,i,x,y) =>" ]
            , Html.input
                [ Attr.value model.input
                , Events.onInput ChangeInput
                , Attr.attribute "autocomplete" "off"
                , Attr.attribute "autocapitalize" "off"
                , Attr.attribute "spellcheck" "false"
                , Attr.attribute "enterkeyhint" "go"
                ]
                []
            ]
        ]
