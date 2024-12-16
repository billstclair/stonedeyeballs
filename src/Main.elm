module Main exposing (main)

import Browser
import Browser.Events as Events exposing (Visibility(..))
import Html exposing (Attribute, Html, a, br, button, div, img, p, text)
import Html.Attributes exposing (height, href, property, src, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra as LE
import Time exposing (Posix)


type alias Model =
    { err : Maybe String
    , sources : List String
    , src : String
    , time : Int
    , lastSwapTime : Int
    , visibility : Visibility
    }


init : ( Model, Cmd Msg )
init =
    ( { err = Nothing
      , sources = [ "stonedeyeballs.jpg" ]
      , src = "stonedeyeballs.jpg"
      , time = 0
      , lastSwapTime = 0
      , visibility = Visible
      }
    , Http.get
        { url = "images/index.json"
        , expect = Http.expectJson GotIndex (JD.list JD.string)
        }
    )


type Msg
    = GotIndex (Result Http.Error (List String))
    | MouseDown
    | ReceiveTime Posix
    | SetVisible Visibility


swapInterval : Int
swapInterval =
    5 * 1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown ->
            ( nextImage model, Cmd.none )

        ReceiveTime posix ->
            let
                millis =
                    Time.posixToMillis posix
            in
            let
                m =
                    { model
                        | time = millis
                        , lastSwapTime =
                            if model.lastSwapTime == 0 then
                                millis

                            else
                                model.lastSwapTime
                    }
            in
            if
                (m.visibility == Visible)
                    && (m.lastSwapTime /= 0)
                    && (millis >= m.lastSwapTime + swapInterval)
            then
                ( nextImage m, Cmd.none )

            else
                ( m, Cmd.none )

        SetVisible v ->
            ( { model | visibility = Debug.log "visibility" v }, Cmd.none )

        GotIndex result ->
            case result of
                Err e ->
                    ( { model | err = Just <| Debug.toString e }
                    , Cmd.none
                    )

                Ok list ->
                    ( { model | sources = Debug.log "sources" list }, Cmd.none )


centerFit : List (Attribute msg)
centerFit =
    [ style "max-width" "100%"
    , style "max-height" "100vh"

    --, style "margin" "auto"
    ]


nextImage : Model -> Model
nextImage model =
    { model
        | src =
            nextElement model.sources model.src
        , lastSwapTime = model.time
    }


nextElement : List String -> String -> String
nextElement strings string =
    case LE.elemIndex string strings of
        Nothing ->
            case LE.getAt 0 strings of
                Just s ->
                    s

                Nothing ->
                    "stonedeyeballs.jpg"

        Just index ->
            let
                idx =
                    if index < List.length strings then
                        index + 1

                    else
                        0
            in
            case LE.getAt idx strings of
                Just s ->
                    s

                Nothing ->
                    "stonedeyeballs.jpg"


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ img
            (List.concat
                [ centerFit
                , [ src <| "images/" ++ model.src
                  , style "text-align" "center"
                  , onClick MouseDown
                  ]
                ]
            )
            []
        , p []
            [ text "Click on the image to change it."
            , br [] []
            , a
                [ href "https://github.com/billstclair/stonedeyeballs"
                , target "_blank"
                ]
                [ text "GitHub" ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onVisibilityChange SetVisible
        , Time.every 100.0 ReceiveTime

        --, Events.onMouseDown mouseDownDecoder
        ]


mouseDownDecoder : Decoder Msg
mouseDownDecoder =
    JD.succeed MouseDown
