module Main exposing (main)

{-| TODO:

Write code for editing the sources.
Merge with `site/index.json` if `mergeWithImageIndex` is true.

-}

import Browser
import Browser.Events as Events exposing (Visibility(..))
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, hr, img, input, p, span, text)
import Html.Attributes exposing (checked, disabled, height, href, property, src, style, target, title, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import PortFunnel.LocalStorage as LocalStorage
import PortFunnels exposing (FunnelDict, Handler(..), State)
import String.Extra as SE
import Time exposing (Posix)


type alias Model =
    { err : Maybe String
    , sources : List String
    , src : String
    , time : Int
    , lastSwapTime : Int
    , visibility : Visibility
    , switchEnabled : Bool
    , showControls : Bool
    , started : Started
    , funnelState : State
    }


type alias SavedModel =
    { sources : List String
    , src : String
    , switchEnabled : Bool
    , showControls : Bool
    }


saveModel : Model -> Cmd Msg
saveModel model =
    put "model"
        (modelToSavedModel model
            |> encodeSavedModel
            |> Just
        )


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { sources = model.sources
    , src = model.src
    , switchEnabled = model.switchEnabled
    , showControls = model.showControls
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | sources = savedModel.sources
        , src = savedModel.src
        , switchEnabled = savedModel.switchEnabled
        , showControls = savedModel.showControls
    }


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "sources" (JD.list JD.string)
        |> required "src" JD.string
        |> required "switchEnabled" JD.bool
        |> optional "showControls" JD.bool False


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "sources", JE.list (\s -> JE.string s) savedModel.sources )
        , ( "src", JE.string savedModel.src )
        , ( "switchEnabled", JE.bool savedModel.switchEnabled )
        , ( "showControls", JE.bool savedModel.showControls )
        ]


init : ( Model, Cmd Msg )
init =
    ( { err = Nothing
      , sources = [ "stoned-eyeballs.jpg" ]
      , src = "stoned-eyeballs.jpg"
      , time = 0
      , lastSwapTime = 0
      , visibility = Visible
      , switchEnabled = True
      , showControls = False
      , started = NotStarted
      , funnelState = initialFunnelState
      }
    , Http.get
        { url = "images/index.json"
        , expect = Http.expectJson GotIndex (JD.list JD.string)
        }
    )



---
--- Persistence
---


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


localStoragePrefix : String
localStoragePrefix =
    "stonedeyeballs"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


type Msg
    = GotIndex (Result Http.Error (List String))
    | MouseDown
    | ReceiveTime Posix
    | SetVisible Visibility
    | OnKeyPress Bool String
    | ToggleSwitchEnabled
    | ToggleControls
    | Process Value


swapInterval : Int
swapInterval =
    5 * 1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doUpdate =
            case msg of
                ReceiveTime _ ->
                    False

                _ ->
                    True

        ( mdl, cmd ) =
            updateInternal msg model
    in
    if doUpdate && mdl.started == Started then
        mdl
            |> withCmds
                [ cmd, saveModel mdl ]

    else
        mdl |> withCmd cmd


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        MouseDown ->
            ( nextImage model, Cmd.none )

        ReceiveTime posix ->
            let
                millis =
                    Time.posixToMillis posix

                lastSwapTime =
                    model.lastSwapTime
            in
            let
                m =
                    { model
                        | time = millis
                        , lastSwapTime =
                            if lastSwapTime == 0 then
                                millis

                            else
                                lastSwapTime
                    }
            in
            if
                (m.visibility == Visible)
                    && m.switchEnabled
                    && (millis >= lastSwapTime + swapInterval)
            then
                ( nextImage { m | lastSwapTime = millis }, Cmd.none )

            else
                ( m, Cmd.none )

        SetVisible v ->
            ( { model | visibility = Debug.log "visibility" v }, Cmd.none )

        OnKeyPress isDown key ->
            if not isDown then
                model |> withNoCmd

            else
            --let
            --k =
            --    Debug.log "KeyDown" key
            --in
            if
                List.member key [ "ArrowLeft", "j", "J", "s", "S" ]
            then
                prevImage model |> withNoCmd

            else if List.member key [ "ArrowRight", "l", "k", "f", "F" ] then
                nextImage model |> withNoCmd

            else if key >= "0" && key <= "9" then
                digitKey key model |> withNoCmd

            else
                model |> withNoCmd

        ToggleSwitchEnabled ->
            ( { model | switchEnabled = not model.switchEnabled }, Cmd.none )

        ToggleControls ->
            { model | showControls = not model.showControls }
                |> withNoCmd

        GotIndex result ->
            case result of
                Err e ->
                    ( { model | err = Just <| Debug.toString e }
                    , Cmd.none
                    )

                Ok list ->
                    ( { model | sources = Debug.log "sources" list }, Cmd.none )

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | err = Just <| Debug.toString error }
                        |> withNoCmd

                Ok res ->
                    res


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    }


type Started
    = NotStarted
    | StartedReadingModel
    | Started


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch
                    [ get pk.model
                    ]

            else
                Cmd.none

        lbl label =
            case label of
                Nothing ->
                    ""

                Just l ->
                    "[" ++ l ++ "] "
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            handleGetResponse label key value mdl

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys mdl

        _ ->
            mdl |> withCmd cmd


handleListKeysResponse : Maybe String -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse maybeLabel prefix keys model =
    case maybeLabel of
        Nothing ->
            model |> withNoCmd

        Just label ->
            model |> withCmds (List.map (getLabeled label) keys)


handleGetResponse : Maybe String -> String -> Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetResponse maybeLabel key maybeValue model =
    case maybeLabel of
        Nothing ->
            if Debug.log "handleGetResponse, key" key == pk.model then
                handleGetModel maybeValue model

            else
                model |> withNoCmd

        Just label ->
            -- This doesn't happen in this app
            case maybeValue of
                Nothing ->
                    model |> withNoCmd

                Just value ->
                    model |> withNoCmd


handleGetModel : Maybe Value -> Model -> ( Model, Cmd Msg )
handleGetModel maybeValue model =
    let
        model2 =
            { model
                | started = Started
                , err = Nothing
            }
    in
    case maybeValue of
        Nothing ->
            model2 |> withNoCmd

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model2
                        | err =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> withNoCmd

                Ok savedModel ->
                    savedModelToModel savedModel model2
                        |> withNoCmd


centerFit : List (Attribute msg)
centerFit =
    [ style "max-width" "100%"
    , style "max-height" "100vh"

    --, style "margin" "auto"
    ]


digitKey : String -> Model -> Model
digitKey digit model =
    let
        index =
            case String.toInt digit of
                Just i ->
                    i

                Nothing ->
                    0
    in
    case LE.getAt index model.sources of
        Just src ->
            { model | src = src }

        Nothing ->
            model


nextImage : Model -> Model
nextImage model =
    viewImage model
        (case LE.elemIndex model.src model.sources of
            Just idx ->
                idx + 1

            Nothing ->
                List.length model.sources - 1
        )


prevImage : Model -> Model
prevImage model =
    viewImage model
        (case LE.elemIndex model.src model.sources of
            Just idx ->
                idx - 1

            Nothing ->
                0
        )


viewImage : Model -> Int -> Model
viewImage model index =
    let
        sources =
            model.sources

        size =
            List.length sources

        idx =
            if index < 0 then
                size - 1

            else if index >= size then
                0

            else
                index
    in
    case LE.getAt idx sources of
        Just s ->
            { model | src = s }

        _ ->
            model


nextElement : List String -> String -> String
nextElement strings string =
    case LE.elemIndex string strings of
        Nothing ->
            case LE.getAt 0 strings of
                Just s ->
                    s

                Nothing ->
                    "stoned-eyeballs.jpg"

        Just index ->
            let
                idx =
                    if index + 1 < List.length strings then
                        index + 1

                    else
                        0
            in
            case LE.getAt idx strings of
                Just s ->
                    s

                Nothing ->
                    "stoned-eyeballs.jpg"


getNameFromFileName : String -> String
getNameFromFileName filename =
    let
        noType =
            SE.leftOfBack "." filename

        name =
            SE.rightOfBack "/" noType

        res =
            String.replace "-" " " <|
                if name == "" then
                    noType

                else
                    name
    in
    SE.toTitleCase res


view : Model -> Html Msg
view model =
    if model.started /= Started then
        text ""

    else
        viewInternal model


viewInternal : Model -> Html Msg
viewInternal model =
    let
        name =
            getNameFromFileName model.src

        index =
            case LE.elemIndex model.src model.sources of
                Just i ->
                    i

                Nothing ->
                    0
    in
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
        , br
        , text name
        , p []
            (List.indexedMap
                (\idx url ->
                    let
                        idxstr =
                            String.fromInt idx

                        idxElements =
                            [ text special.nbsp
                            , text idxstr
                            , text special.nbsp
                            ]

                        idxName =
                            getNameFromFileName url
                    in
                    if idx == index then
                        span [ title idxName ] <|
                            idxElements
                                ++ [ text " " ]

                    else
                        span []
                            [ a
                                [ href "#"
                                , onClick <| OnKeyPress True idxstr
                                , style "text-decoration" "none"
                                , title idxName
                                ]
                                [ span [ style "text-decoration" "underline" ]
                                    idxElements
                                ]
                            , text " "
                            ]
                )
                model.sources
            )
        , p []
            [ text "Click on the image to change. Or press s/f, j/l, digit, arrows, or digit links above."
            , br
            , checkBox ToggleSwitchEnabled
                model.switchEnabled
                "Auto-switch images"
            , br
            , a
                [ href "https://github.com/billstclair/stonedeyeballs"
                , target "_blank"
                ]
                [ text "GitHub" ]
            , text " "
            , a
                [ href "https://stoneder.club"
                , target "_blank"
                ]
                [ text "Stoneder.club" ]
            , p []
                [ text "Copyright "
                , text special.copyright
                , text "2024, "
                , a
                    [ href "https://billstclair.com/"
                    , target "_blank"
                    ]
                    [ text "Bill St. Clair" ]
                ]
            , br
            , button ToggleControls <|
                if model.showControls then
                    "Hide Controls"

                else
                    "Show Controls"
            , if model.showControls then
                p []
                    [ hr [] []
                    , viewControls model
                    ]

              else
                text ""
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    text "Controls"


titledButton : String -> Bool -> Msg -> String -> Html Msg
titledButton theTitle enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        , title theTitle
        , style "border-radius" "9999px"
        , style "border-width" "1px"
        ]
        [ b label ]


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton =
    titledButton ""


button : Msg -> String -> Html Msg
button =
    enabledButton True


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    , biohazard = stringFromCode 9763 -- \u2623
    , black_star = stringFromCode 10036 -- \u2734
    , hourglass = stringFromCode 8987 -- \u231B
    , hourglass_flowing = stringFromCode 9203 -- \u23F3
    , checkmark = stringFromCode 10003 -- \u2713
    , middleDot = stringFromCode 183 -- \u00B7
    }


titledCheckBox : String -> Msg -> Bool -> String -> Html Msg
titledCheckBox theTitle msg isChecked label =
    span
        [ onClick msg
        , style "cursor" "default"
        , title theTitle
        , style "white-space" "nowrap"
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            ]
            []
        , b label
        ]


checkBox : Msg -> Bool -> String -> Html Msg
checkBox =
    titledCheckBox ""


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


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
        , Events.onKeyDown <| keyDecoder True
        , Events.onKeyUp <| keyDecoder False
        , Time.every 100.0 ReceiveTime
        , PortFunnels.subscriptions Process model

        --, Events.onMouseDown mouseDownDecoder
        ]


keyDecoder : Bool -> Decoder Msg
keyDecoder keyDown =
    JD.field "key" JD.string
        |> JD.map (OnKeyPress keyDown)


mouseDownDecoder : Decoder Msg
mouseDownDecoder =
    JD.succeed MouseDown
