module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, img, text)
import Html.Attributes exposing (height, property, src)
import Html.Events exposing (onClick)
import Json.Encode as JE


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


textProperty : String -> String -> Attribute msg
textProperty name value =
    property name <| JE.string value


centerFit : List (Attribute msg)
centerFit =
    [ textProperty "max-width" "100%"
    , textProperty "max-height" "100vh"
    , textProperty "margin" "auto"
    ]


view : Model -> Html Msg
view model =
    div []
        [ img
            (List.concat
                [ [ src "images/stoned-eyeballs-2-web.jpg" ]
                , centerFit
                ]
            )
            []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
