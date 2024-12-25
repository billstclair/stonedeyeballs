module Main exposing (main)

import Browser
import Browser.Events as Events exposing (Visibility(..))
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, img, input, p, span, text)
import Html.Attributes exposing (checked, height, href, property, src, style, target, title, type_)
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
    , started : Started
    , funnelState : State
    }


type alias SavedModel =
    { sources : List String
    , src : String
    , switchEnabled : Bool
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
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | sources = savedModel.sources
        , src = savedModel.src
        , switchEnabled = savedModel.switchEnabled
    }


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "sources" (JD.list JD.string)
        |> required "src" JD.string
        |> required "switchEnabled" JD.bool


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "sources", JE.list (\s -> JE.string s) savedModel.sources )
        , ( "src", JE.string savedModel.src )
        , ( "switchEnabled", JE.bool savedModel.switchEnabled )
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
    | ToggleSwitchEnabled
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
                    && m.switchEnabled
                    && (m.lastSwapTime /= 0)
                    && (millis >= m.lastSwapTime + swapInterval)
            then
                ( nextImage m, Cmd.none )

            else
                ( m, Cmd.none )

        SetVisible v ->
            ( { model | visibility = Debug.log "visibility" v }, Cmd.none )

        ToggleSwitchEnabled ->
            ( { model | switchEnabled = not model.switchEnabled }, Cmd.none )

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
        , let
            name =
                getNameFromFileName model.src
          in
          text name
        , p []
            [ text "Click on the image to change it."
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
            ]
        ]


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
        , Time.every 100.0 ReceiveTime
        , PortFunnels.subscriptions Process model

        --, Events.onMouseDown mouseDownDecoder
        ]


mouseDownDecoder : Decoder Msg
mouseDownDecoder =
    JD.succeed MouseDown
