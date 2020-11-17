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
import Svg
import Svg.Attributes as SvgAttr
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

        firstTutorial =
            Zipper.current allTutorials

        fromUrl =
            case Url.Parser.parse routeParser url of
                Just (Root (Just value)) ->
                    Just value

                _ ->
                    Nothing

        initialInput =
            Maybe.withDefault firstTutorial.source fromUrl
    in
    { time = 0
    , input = initialInput
    , expression = parse initialInput
    , help = firstTutorial.description
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
                -- we want to keep the last parseable expression so that the UI
                -- doesn't flicker too much while the user is editing.
                -- Only exception is when the whole input is cleared.
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
            ( model
            , Navigation.replaceUrl model.key
                (Url.Builder.absolute []
                    [ Url.Builder.string "code" model.input ]
                )
            )

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


callTixyFn : Maybe Expression -> { t : Float, i : Int, x : Int, y : Int } -> Float
callTixyFn exp { t, i, x, y } =
    let
        tixy =
            { t = t / 1000
            , i = toFloat i
            , x = toFloat x
            , y = toFloat y
            }

        result =
            exp
                |> Maybe.map (Expression.evaluate tixy)
                |> Maybe.withDefault 0
    in
    clamp -1 1 result


inputId : String
inputId =
    "tixy-input"


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.h1 [] [ text "tixy.elm" ]
        , div [ class "map", Html.Events.onClick NextTutorial ]
            [ viewSvg model.expression model.time ]
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



-- SVG


size : Float
size =
    320


gridSize : Int
gridSize =
    16


cellSize : Float
cellSize =
    size / toFloat gridSize


viewSvg : Maybe Expression -> Float -> Html msg
viewSvg expr t =
    let
        size_ =
            String.fromFloat size
    in
    Svg.svg
        [ SvgAttr.width size_
        , SvgAttr.height size_
        , SvgAttr.viewBox ("0 0 " ++ size_ ++ " " ++ size_)
        ]
        (Svg.rect
            [ SvgAttr.x "0"
            , SvgAttr.y "0"
            , SvgAttr.width size_
            , SvgAttr.height size_
            , SvgAttr.fill "#111"
            ]
            []
            :: (List.range 0 255 |> List.map (renderCircle expr t))
        )


renderCircle expr t index =
    let
        col =
            modBy gridSize index

        row =
            index // gridSize

        ( x, y ) =
            ( (toFloat col * cellSize) + cellSize / 2
            , (toFloat row * cellSize) + cellSize / 2
            )

        result =
            callTixyFn expr { t = t, i = index, x = col, y = row }

        fillColor =
            if result < 0 then
                "#f24"

            else
                "white"

        -- we want the maximum size of the circle to be slightly smaller
        -- so the grid has a little bit of breathing room.
        radius =
            cellSize / 2 * 0.98 * abs result
    in
    Svg.circle
        [ SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r (String.fromFloat radius)
        , SvgAttr.fill fillColor
        ]
        []
