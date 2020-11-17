module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Navigation
import Expression exposing (Expression)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (attribute, class)
import Html.Events
import Json.Decode
import List.Zipper as Zipper exposing (Zipper)
import Parser
import Task
import Time
import Tutorial exposing (Tutorial)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>), top)
import Url.Parser.Query as Query


type alias Model =
    { time : Float
    , input : String
    , expression : Maybe Expression
    , help : String
    , tutorials : Zipper Tutorial
    , key : Navigation.Key
    }


type Route
    = Root (Maybe String)


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.map Root (top <?> Query.string "code")


init : Url -> Navigation.Key -> Model
init url key =
    let
        allTutorials =
            Tutorial.all

        { description, source } =
            Zipper.current allTutorials

        fromUrl =
            case Url.Parser.parse routeParser url of
                Just (Root (Just value)) ->
                    Just value

                _ ->
                    Nothing

        initialInput =
            Maybe.withDefault source fromUrl
    in
    { time = 0
    , input = initialInput
    , expression = parse initialInput
    , help = description
    , tutorials = allTutorials
    , key = key
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
    | FocusInput
    | InputFocused
    | InputKeyDown Int
    | NoOp


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

        FocusInput ->
            ( model, Task.attempt (\_ -> InputFocused) (Dom.focus inputId) )

        InputFocused ->
            ( { model | help = "hit ENTER to update the url\nso you can share it" }, Cmd.none )

        InputKeyDown 13 ->
            let
                url =
                    Url.Builder.absolute []
                        [ Url.Builder.string "code" model.input ]
            in
            ( model, Navigation.replaceUrl model.key url )

        InputKeyDown _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = \flags url key -> ( init url key, Cmd.none )
        , view =
            \model ->
                { title = "tixy.elm"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Events.onAnimationFrameDelta Tick
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
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


inputId : String
inputId =
    "tixy-input"


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
                , Attr.id inputId
                , Html.Events.onInput ChangeInput
                , Html.Events.onClick InputFocused
                , onKeyDown InputKeyDown
                , Attr.attribute "autocomplete" "off"
                , Attr.attribute "autocapitalize" "off"
                , Attr.attribute "spellcheck" "false"
                , Attr.attribute "enterkeyhint" "go"
                ]
                []
            ]
        ]


viewHelp : String -> Html Msg
viewHelp str =
    div [ class "help", Html.Events.onClick FocusInput ]
        (str
            |> String.split "\n"
            |> List.map
                (\line ->
                    div [ class "help-line" ] [ text ("// " ++ line) ]
                )
        )


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (Json.Decode.map tagger Html.Events.keyCode)
