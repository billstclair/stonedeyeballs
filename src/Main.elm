module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, img, text)
import Html.Attributes exposing (height, property, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE


type alias Model =
    { err : Maybe String
    , sources : List String
    , src : String
    }


init : ( Model, Cmd Msg )
init =
    ( { err = Nothing
      , sources = [ "stoned-eyeballs-2-web.jpg" ]
      , src = "stonedeyeballs.jpg"
      }
    , Http.get
        { url = "images/index.json"
        , expect = Http.expectJson GotIndex (JD.list JD.string)
        }
    )


type Msg
    = GotIndex (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                  ]
                ]
            )
            []
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }
