module Main exposing (main)

{-| TODO:

Add save/restore area for lists of urls.

Optional URL to go with image, defaults to the image itself.

-}

import AssocSet as AS exposing (Set)
import Browser
import Browser.Events as Events exposing (Visibility(..))
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, div, embed, hr, img, input, p, span, table, td, text, textarea, tr)
import Html.Attributes exposing (checked, class, disabled, height, href, property, src, style, target, title, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import PortFunnel.LocalStorage as LocalStorage
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Process
import String.Extra as SE
import Task exposing (Task, succeed)
import Time exposing (Posix)
import Url


type alias Source =
    { src : String
    , label : Maybe String
    }


sourceLabel : Source -> String
sourceLabel s =
    case s.label of
        Just label ->
            label

        Nothing ->
            getNameFromFileName s.src


source : String -> Maybe String -> Source
source =
    Source


type alias Model =
    { err : Maybe String
    , sources : List Source
    , lastSources : List Source
    , src : String
    , editingSources : List Source
    , editingSrc : String
    , sourcePanels : Dict String (List Source)
    , justAddedEditingRow : Bool
    , time : Int
    , lastSwapTime : Int
    , reallyDeleteState : Bool
    , visibility : Visibility
    , switchPeriod : String
    , switchEnabled : Bool
    , showControls : Bool
    , showEditingSources : Bool
    , mergeEditingSources : Bool
    , controlsJson : String
    , started : Started
    , funnelState : State
    }


type alias SavedModel =
    { sources : List Source
    , lastSources : List Source
    , src : String
    , editingSources : List Source
    , editingSrc : String
    , sourcePanels : Dict String (List Source)
    , switchPeriod : String
    , switchEnabled : Bool
    , showControls : Bool
    , showEditingSources : Bool
    , mergeEditingSources : Bool
    }


saveModel : Model -> Cmd Msg
saveModel model =
    put "model"
        (modelToSavedModel model
            --|> Debug.log "  SavedModel"
            |> encodeSavedModel
            |> Just
        )


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { sources = model.sources
    , lastSources = model.lastSources
    , src = model.src
    , editingSources = model.editingSources
    , editingSrc = model.editingSrc
    , sourcePanels = model.sourcePanels
    , switchPeriod = model.switchPeriod
    , switchEnabled = model.switchEnabled
    , showControls = model.showControls
    , showEditingSources = model.showEditingSources
    , mergeEditingSources = model.mergeEditingSources
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | sources = savedModel.sources
        , lastSources = savedModel.lastSources
        , src = savedModel.src
        , editingSources = savedModel.editingSources
        , editingSrc = savedModel.editingSrc
        , sourcePanels = savedModel.sourcePanels
        , switchPeriod = savedModel.switchPeriod
        , switchEnabled = savedModel.switchEnabled
        , showControls = savedModel.showControls
        , showEditingSources = savedModel.showEditingSources
        , mergeEditingSources = savedModel.mergeEditingSources
    }
        |> updateControlsJson


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "sources" sourcesDecoder
        |> optional "lastSources" sourcesDecoder []
        |> required "src" JD.string
        |> optional "editingSources" sourcesDecoder []
        |> optional "editingSrc" JD.string ""
        |> optional "sourcePanels" (JD.dict (JD.list sourceDecoder)) Dict.empty
        |> optional "switchPeriod" JD.string "5"
        |> required "switchEnabled" JD.bool
        |> optional "showControls" JD.bool False
        |> optional "showEditingSources" JD.bool True
        |> optional "mergeEditingSources" JD.bool True


sourcesDecoder : Decoder (List Source)
sourcesDecoder =
    JD.list sourceDecoder


sourceDecoder : Decoder Source
sourceDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s -> srcSource s |> JD.succeed)
        , JD.succeed Source
            |> required "src" JD.string
            |> optional "label" (JD.nullable JD.string) Nothing
        ]


encodeSource : Source -> Value
encodeSource { src, label } =
    if label == Nothing then
        JE.string src

    else
        JE.object
            [ ( "src", JE.string src )
            , ( "label"
              , case label of
                    Just l ->
                        if
                            (l == "")
                                || (l == (srcSource src |> sourceLabel))
                        then
                            JE.null

                        else
                            JE.string l

                    Nothing ->
                        JE.null
              )
            ]


encodeSavedModel : SavedModel -> Value
encodeSavedModel savedModel =
    JE.object
        [ ( "sources", JE.list encodeSource savedModel.sources )
        , ( "lastSources", JE.list encodeSource savedModel.lastSources )
        , ( "src", JE.string savedModel.src )
        , ( "editingSources", JE.list encodeSource savedModel.editingSources )
        , ( "editingSrc", JE.string savedModel.editingSrc )
        , ( "sourcePanels", JE.dict identity (JE.list encodeSource) savedModel.sourcePanels )
        , ( "switchPeriod", JE.string savedModel.switchPeriod )
        , ( "switchEnabled", JE.bool savedModel.switchEnabled )
        , ( "showControls", JE.bool savedModel.showControls )
        , ( "showEditingSources", JE.bool savedModel.showEditingSources )
        , ( "mergeEditingSources", JE.bool savedModel.mergeEditingSources )
        ]


stonedEyeballsUrl : String
stonedEyeballsUrl =
    "stoned-eyeballs.jpg"


stonedEyeballsSource : Source
stonedEyeballsSource =
    srcSource stonedEyeballsUrl


srcSource : String -> Source
srcSource src =
    { src = src, label = Nothing }


init : ( Model, Cmd Msg )
init =
    ( { err = Nothing
      , sources = [ stonedEyeballsSource ]
      , lastSources = [ stonedEyeballsSource ]
      , src = stonedEyeballsUrl
      , editingSources = []
      , editingSrc = ""
      , sourcePanels = Dict.fromList []
      , justAddedEditingRow = False
      , time = 0
      , lastSwapTime = 0
      , reallyDeleteState = False
      , visibility = Visible
      , switchPeriod = "5"
      , switchEnabled = True
      , showControls = False
      , showEditingSources = True
      , mergeEditingSources = True
      , controlsJson = "[]"
      , started = NotStarted
      , funnelState = initialFunnelState
      }
    , Cmd.none
    )


computeControlsJson : List Source -> String
computeControlsJson sources =
    JE.encode 2 <| JE.list encodeSource sources


updateControlsJson : Model -> Model
updateControlsJson model =
    { model | controlsJson = computeControlsJson model.editingSources }



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


clearKeys : String -> Cmd Msg
clearKeys prefix =
    localStorageSend (LocalStorage.clear prefix)


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
    = Noop
    | GotIndex Bool (Result Http.Error (List String))
    | MouseDown
    | ReceiveTime Posix
    | SetVisible Visibility
    | OnKeyPress Bool Bool String
    | InputSwitchPeriod String
    | ToggleSwitchEnabled
    | ToggleControls
    | ToggleShowEditingSources
    | ToggleMergeEditingSources
    | SelectEditingSrc String
    | AddEditingSrc
    | DeleteEditingSrc
    | DisplayEditingSrc
    | InputEditingSrc String
    | InputEditingName String
    | SaveRestoreEditingSources Bool
    | SaveRestoreControlsJson Bool
    | DeleteAllEditingSources
    | InputControlsJson String
    | AddSourcePanel
    | ReloadFromServer
    | DeleteState
    | Process Value


swapInterval : Model -> Int
swapInterval model =
    (case String.toFloat model.switchPeriod of
        Just f ->
            max 1 f

        Nothing ->
            5
    )
        * 1000
        |> round


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doUpdate =
            case msg of
                ReceiveTime _ ->
                    False

                DeleteState ->
                    False

                Process _ ->
                    False

                GotIndex _ _ ->
                    False

                SetVisible _ ->
                    False

                _ ->
                    True

        preserveJustAddedEditingRow =
            case msg of
                InputEditingSrc _ ->
                    True

                _ ->
                    False

        ( mdl, cmd ) =
            updateInternal doUpdate preserveJustAddedEditingRow msg model
    in
    if doUpdate && mdl.started == Started && mdl /= model then
        let
            m =
                Debug.log "update, msg" msg
        in
        mdl
            |> withCmds
                [ cmd, saveModel mdl ]

    else
        mdl |> withCmd cmd


updateInternal : Bool -> Bool -> Msg -> Model -> ( Model, Cmd Msg )
updateInternal doUpdate preserveJustAddedEditingRow msg modelIn =
    let
        modelIn1 =
            if msg == DeleteState || not doUpdate then
                modelIn

            else
                { modelIn | reallyDeleteState = False }

        modelIn2 =
            if not preserveJustAddedEditingRow then
                { modelIn1 | justAddedEditingRow = False }

            else
                modelIn1

        model =
            if modelIn2.editingSources == [] then
                { modelIn2 | editingSources = modelIn2.sources }
                    |> updateControlsJson

            else
                modelIn2
    in
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
                    && (millis >= m.lastSwapTime + swapInterval m)
            then
                ( nextImage { m | lastSwapTime = millis }, Cmd.none )

            else
                ( m, Cmd.none )

        SetVisible v ->
            ( { model | visibility = Debug.log "visibility" v }, Cmd.none )

        OnKeyPress isDown doDigits key ->
            if not isDown then
                model |> withNoCmd

            else
            --let
            --k =
            --    Debug.log "KeyDown" key
            --in
            if
                List.member key [ "ArrowLeft", "j", "J", "d", "s", "S" ]
            then
                prevImage model |> withNoCmd

            else if List.member key [ "ArrowRight", "l", "k", "f", "F" ] then
                nextImage model |> withNoCmd

            else if doDigits && key >= "0" && key <= "9" then
                digitKey key model |> withNoCmd

            else
                model |> withNoCmd

        InputSwitchPeriod string ->
            { model | switchPeriod = string }
                |> withNoCmd

        ToggleSwitchEnabled ->
            ( { model
                | switchEnabled = not model.switchEnabled
                , lastSwapTime = model.time
              }
            , Cmd.none
            )

        ToggleControls ->
            { model
                | showControls =
                    not model.showControls
                , editingSrc =
                    Debug.log "ToggleControls, editingSrc" <|
                        if model.showControls then
                            model.editingSrc

                        else
                            model.src
            }
                |> withNoCmd

        ToggleShowEditingSources ->
            { model | showEditingSources = not model.showEditingSources }
                |> withNoCmd

        ToggleMergeEditingSources ->
            { model | mergeEditingSources = not model.mergeEditingSources }
                |> withNoCmd

        SelectEditingSrc src ->
            { model
                | editingSrc = Debug.log "SelectEditingSrc" src
                , src = src
            }
                |> withNoCmd

        AddEditingSrc ->
            let
                ( head, tail ) =
                    headTail model.editingSrc model.editingSources
            in
            { model
                | editingSources = head ++ [ srcSource "" ] ++ tail
                , editingSrc = ""
                , justAddedEditingRow = True
            }
                |> updateControlsJson
                |> withCmd
                    (delay 1 <| SelectEditingSrc "")

        DeleteEditingSrc ->
            { model
                | editingSources =
                    Debug.log "DeleteEditingSrc, sources:" <|
                        List.filter (\s -> s.src /= model.editingSrc) model.editingSources
                , editingSrc = ""
            }
                |> updateControlsJson
                |> withNoCmd

        DisplayEditingSrc ->
            { model | src = model.editingSrc }
                |> withNoCmd

        InputEditingSrc editingSrc ->
            let
                _ =
                    Debug.log "InputEditingSrc" editingSrc
            in
            case LE.findIndex (\a -> a.src == model.editingSrc) model.editingSources of
                Nothing ->
                    { model
                        | editingSrc = editingSrc
                        , src = editingSrc
                    }
                        |> withNoCmd

                Just editingSrcIdx ->
                    let
                        editingSources =
                            LE.setAt editingSrcIdx
                                (srcSource editingSrc)
                                model.editingSources
                    in
                    { model
                        | editingSrc = editingSrc
                        , editingSources = editingSources
                        , src = editingSrc
                    }
                        |> updateControlsJson
                        |> withNoCmd

        InputEditingName name ->
            (case LE.findIndex (\s -> s.src == model.editingSrc) model.editingSources of
                Just i ->
                    case LE.getAt i model.editingSources of
                        Just s ->
                            { model
                                | editingSources =
                                    LE.setAt i
                                        { s | label = Just name }
                                        model.editingSources
                            }
                                |> updateControlsJson

                        Nothing ->
                            model

                Nothing ->
                    model
            )
                |> withNoCmd

        SaveRestoreControlsJson savep ->
            if savep then
                case JD.decodeString sourcesDecoder model.controlsJson of
                    Ok sources ->
                        { model | editingSources = sources }
                            |> withNoCmd

                    Err e ->
                        { model | err = Just <| JD.errorToString e }
                            |> withNoCmd

            else
                { model | controlsJson = computeControlsJson model.editingSources }
                    |> withNoCmd

        SaveRestoreEditingSources savep ->
            if savep then
                { model
                    | sources = model.editingSources
                    , src = model.editingSrc
                }
                    |> withNoCmd

            else
                { model
                    | editingSources = model.sources
                    , editingSrc = model.src
                }
                    |> updateControlsJson
                    |> withNoCmd

        DeleteAllEditingSources ->
            { model | editingSources = [ srcSource "" ] }
                |> updateControlsJson
                |> withNoCmd

        InputControlsJson string ->
            { model | controlsJson = string }
                |> withNoCmd

        AddSourcePanel ->
            let
                name =
                    newSourcePanelName model.sourcePanels
            in
            { model
                | sourcePanels =
                    Dict.insert name [] model.sourcePanels
            }
                |> withNoCmd

        ReloadFromServer ->
            model
                |> withCmd Navigation.reloadAndSkipCache

        DeleteState ->
            if model.reallyDeleteState then
                let
                    ( mdl, cmd ) =
                        init
                in
                { mdl
                    | reallyDeleteState = False
                    , started = Started
                    , editingSources = []
                    , editingSrc = ""
                }
                    |> updateControlsJson
                    |> withCmds [ cmd, clearKeys "", getIndexJson True ]

            else
                { model | reallyDeleteState = True }
                    |> withNoCmd

        Noop ->
            model |> withNoCmd

        GotIndex setSourcesList result ->
            case result of
                Err e ->
                    ( { model | err = Just <| Debug.toString e }
                    , Cmd.none
                    )

                Ok list ->
                    let
                        sources =
                            Debug.log "GotIndex, sources" <|
                                List.map srcSource list

                        mdl =
                            if setSourcesList then
                                { model | sources = sources }

                            else
                                model
                                    |> maybeAddNewSources sources
                    in
                    mdl |> withNoCmd

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


newSourcePanelName : Dict String (List Source) -> String
newSourcePanelName panelDict =
    "new"


maybeAddNewSources : List Source -> Model -> Model
maybeAddNewSources sources model =
    if not model.mergeEditingSources then
        model

    else
        let
            sourcesSet =
                AS.fromList sources

            lastSourcesSet =
                AS.fromList model.lastSources

            newSourcesSet =
                AS.diff sourcesSet lastSourcesSet

            newSources =
                AS.toList newSourcesSet

            editingSources =
                case model.editingSources of
                    [] ->
                        model.sources

                    s ->
                        s
        in
        { model
            | editingSources = editingSources ++ newSources
            , lastSources = model.lastSources ++ newSources
        }
            |> updateControlsJson


delay : Int -> msg -> Cmd msg
delay millis msg =
    Process.sleep (millis |> toFloat)
        |> Task.perform (\_ -> msg)


cdr : List a -> List a
cdr list =
    case List.tail list of
        Just tail ->
            tail

        Nothing ->
            []


dropN : Int -> List a -> List a
dropN n list =
    if n <= 0 then
        list

    else
        dropN (n - 1) <| cdr list


headTail : String -> List Source -> ( List Source, List Source )
headTail editingSrc editingSources =
    -- TODO
    let
        head =
            LE.takeWhile (\a -> a.src /= editingSrc) editingSources
    in
    case dropN (List.length head) editingSources of
        [] ->
            ( head, [] )

        [ es ] ->
            ( head ++ [ es ], [] )

        es :: rest ->
            ( head ++ [ es ], rest )


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


type alias GetArgsJson msg =
    { url : String, expect : Http.Expect msg, headers : List Http.Header }


httpGet : GetArgsJson msg -> Cmd msg
httpGet args =
    Http.request
        { method = "GET"
        , headers = args.headers
        , url = args.url
        , body = Http.emptyBody
        , expect = args.expect
        , timeout = Nothing
        , tracker = Nothing
        }


getIndexJson : Bool -> Cmd Msg
getIndexJson setSourceList =
    let
        s =
            Debug.log "getIndexJson" setSourceList
    in
    httpGet
        { url = "images/index.json"
        , expect =
            Http.expectJson
                (GotIndex setSourceList)
                (JD.list JD.string)
        , headers =
            [ Http.header "Cache-control" "no-cache, no-store" ]
        }


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
            let
                s =
                    Debug.log "null model value, getIndexJson" True
            in
            model2 |> withCmd (getIndexJson True)

        Just value ->
            case JD.decodeValue savedModelDecoder value of
                Err err ->
                    { model2
                        | err =
                            Just <|
                                Debug.log "Error decoding SavedModel"
                                    (JD.errorToString err)
                    }
                        |> withCmd (getIndexJson True)

                Ok savedModel ->
                    savedModelToModel savedModel model2
                        |> withCmd (getIndexJson False)


centerFit : List (Attribute msg)
centerFit =
    [ style "max-width" "100%"
    , style "max-height" "100vh"

    --, style "margin" "auto"
    ]


digitKey : String -> Model -> Model
digitKey digit modelIn =
    let
        index =
            case String.toInt digit of
                Just i ->
                    i

                Nothing ->
                    0

        model =
            { modelIn | lastSwapTime = modelIn.time }
    in
    case LE.getAt index model.sources of
        Just { src } ->
            { model | src = src }

        Nothing ->
            model


nextImage : Model -> Model
nextImage model =
    viewImage model
        (case LE.findIndex (\idx -> idx.src == model.src) model.sources of
            Just idx ->
                idx + 1

            Nothing ->
                List.length model.sources - 1
        )


prevImage : Model -> Model
prevImage model =
    viewImage model
        (case LE.findIndex (\src -> src.src == model.src) model.sources of
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
        Just { src } ->
            { model | src = src }

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
                    stonedEyeballsUrl

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
                    stonedEyeballsUrl


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


center : List (Attribute msg) -> List (Html msg) -> Html msg
center =
    Html.node "center"


view : Model -> Html Msg
view model =
    if model.started /= Started then
        text ""

    else
        viewInternal model


modelLabel : Model -> String
modelLabel model =
    case LE.find (\s -> s.src == model.src) model.sources of
        Just s ->
            case s.label of
                Just l ->
                    l

                _ ->
                    getNameFromFileName s.src

        Nothing ->
            ""


urlType : String -> String
urlType url =
    String.split "." url
        |> List.reverse
        |> (\l ->
                case List.head l of
                    Nothing ->
                        ""

                    Just res ->
                        res
           )


imgTypes : List String
imgTypes =
    String.split "," "jpg,jpeg,gif,png,svg,webp"


viewInternal : Model -> Html Msg
viewInternal model =
    let
        name =
            modelLabel model

        index =
            case LE.findIndex (\src -> src.src == model.src) model.sources of
                Just i ->
                    i

                Nothing ->
                    0

        modelSrc =
            if String.startsWith "http" model.src then
                model.src

            else
                "images/" ++ model.src

        fileUrlType =
            urlType modelSrc

        isImage =
            List.member fileUrlType imgTypes
    in
    div
        [ style "text-align" "center"
        , style "margin" "auto"
        , style "max-height" "60em"
        , style "overflow" "auto"
        ]
        [ (if isImage then
            img

           else
            embed
          )
            ([ [ src modelSrc
               , style "text-align" "center"
               , onClick MouseDown
               , if not isImage then
                    style "height" "max-content"

                 else
                    style "" ""
               , style "max-height" "500px"
               ]
             , if isImage then
                centerFit

               else
                []
             ]
                |> List.concat
            )
            []
        , br
        , text (String.fromInt index)
        , text ": "
        , a
            [ href modelSrc
            , target "_blank"
            ]
            [ text <| name ++ " (" ++ fileUrlType ++ ")" ]
        , p []
            (List.indexedMap
                (\idx s ->
                    let
                        idxstr =
                            String.fromInt idx

                        idxElements =
                            [ text special.nbsp
                            , text idxstr
                            , text special.nbsp
                            ]

                        idxName =
                            case s.label of
                                Just n ->
                                    n

                                Nothing ->
                                    getNameFromFileName s.src
                    in
                    if idx == index then
                        span [ title idxName ] <|
                            idxElements
                                ++ [ text " " ]

                    else
                        span []
                            [ a
                                [ href "#"
                                , onClick <| OnKeyPress True True idxstr
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
            [ text "Click on the image to change. Or press s/f, j/l, arrows, or digit links above."
            , br
            , checkBox ToggleSwitchEnabled
                model.switchEnabled
                "Auto-switch images"
            , b ", period: "
            , input
                [ onInput InputSwitchPeriod
                , value model.switchPeriod
                , width 2
                , style "min-height" "1em"
                , style "width" "2em"
                ]
                [ text model.switchPeriod ]
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


h1 : String -> Html Msg
h1 title =
    Html.h1 [] [ text title ]


h2 : String -> Html Msg
h2 title =
    Html.h2 [] [ text title ]


h3 : String -> Html Msg
h3 title =
    Html.h3 [] [ text title ]


viewControls : Model -> Html Msg
viewControls model =
    center []
        [ div []
            [ h2 "Controls"
            , p []
                [ checkBox ToggleMergeEditingSources
                    model.mergeEditingSources
                    "Merge new app images with your list."
                ]
            , viewEditingSources model
            , viewSourcePanels model
            , p []
                [ button ReloadFromServer "Reload code from server" ]
            , p []
                [ if model.reallyDeleteState then
                    button DeleteState "Really Delete State"

                  else
                    button DeleteState "Delete State (after confirmation)"
                ]
            ]
        ]


viewSaveDeleteButtons : Model -> Html Msg
viewSaveDeleteButtons model =
    let
        different =
            model.sources /= model.editingSources
    in
    p []
        [ enabledButton different
            (SaveRestoreEditingSources True)
            "Save"
        , text " "
        , button DeleteAllEditingSources
            "Delete all"
        , text " "
        , enabledButton different
            (SaveRestoreEditingSources False)
            "Restore"
        ]


viewEditingSources : Model -> Html Msg
viewEditingSources model =
    span []
        [ button ToggleShowEditingSources <|
            if model.showEditingSources then
                "Hide sources"

            else
                "Show sources"
        , br
        , if not model.showEditingSources then
            text ""

          else
            span []
                [ viewSaveDeleteButtons model
                , viewEditingSourcesInternal model
                , viewSaveDeleteButtons model
                , let
                    controlsJson =
                        computeControlsJson model.editingSources

                    sourcesResult =
                        JD.decodeString sourcesDecoder model.controlsJson

                    saveOk =
                        case sourcesResult of
                            Ok _ ->
                                True

                            Err _ ->
                                False
                  in
                  p []
                    [ enabledButton (saveOk && (sourcesResult /= Ok model.editingSources))
                        (SaveRestoreControlsJson True)
                        "Save"
                    , text " "
                    , enabledButton (controlsJson /= model.controlsJson)
                        (SaveRestoreControlsJson False)
                        "Restore"
                    , br
                    , textarea
                        [ style "min-height" "20em"
                        , style "width" "95%"
                        , onInput InputControlsJson
                        , value model.controlsJson
                        ]
                        [ text model.controlsJson ]
                    ]
                ]
        ]


viewSourcePanels : Model -> Html Msg
viewSourcePanels model =
    span []
        [ h3 "Source Panels"
        , p []
            [ p [] [ button AddSourcePanel "Add" ]
            , p []
                [ table [ class "prettyTable" ] <|
                    let
                        viewRow html =
                            tr [] [ td [] [ html ] ]
                    in
                    Dict.foldr
                        (\name _ res ->
                            (viewRow <| viewSourcePanel model name)
                                :: res
                        )
                        []
                        model.sourcePanels
                ]
            ]
        ]


viewSourcePanel : Model -> String -> Html Msg
viewSourcePanel model name =
    span []
        [ text " "
        , text name
        ]


viewEditingSourcesInternal : Model -> Html Msg
viewEditingSourcesInternal model =
    let
        sources =
            case model.editingSources of
                [] ->
                    model.sources

                s ->
                    s
    in
    span []
        [ table [ class "prettytable" ] <|
            List.indexedMap
                (\idx s ->
                    tr
                        [ onClick <|
                            if not model.justAddedEditingRow then
                                SelectEditingSrc s.src

                            else
                                Noop
                        ]
                        [ td []
                            [ span []
                                [ text <| String.fromInt idx
                                , text ": "
                                , if s.src == model.editingSrc then
                                    span []
                                        [ button AddEditingSrc "Add"
                                        , text " "
                                        , button DeleteEditingSrc "Delete"
                                        , text " "
                                        , if model.src /= model.editingSrc then
                                            span []
                                                [ button DisplayEditingSrc "Display"
                                                , text " "
                                                ]

                                          else
                                            text ""
                                        , input
                                            [ onInput InputEditingSrc
                                            , width 30
                                            , value s.src
                                            , style "min-height" "1em"
                                            , style "min-width" "20em"
                                            ]
                                            [ text s.src ]
                                        , text " "
                                        , input
                                            [ onInput InputEditingName
                                            , width 20
                                            , value <| sourceLabel s
                                            ]
                                            [ text s.src ]
                                        ]

                                  else
                                    span
                                        [ style "min-height" "1em"
                                        , style "min-width" "30em"
                                        ]
                                        [ text <|
                                            if s.src == "" then
                                                special.nbsp

                                            else
                                                s.src
                                        ]
                                ]
                            ]
                        ]
                )
                sources
        ]


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
        , Events.onKeyUp <| keyDecoder False
        , Time.every 100.0 ReceiveTime
        , PortFunnels.subscriptions Process model
        , Events.onKeyDown <| keyDecoder True

        --, Events.onMouseDown mouseDownDecoder
        ]


keyDecoder : Bool -> Decoder Msg
keyDecoder keyDown =
    JD.field "key" JD.string
        |> JD.map (OnKeyPress keyDown False)


mouseDownDecoder : Decoder Msg
mouseDownDecoder =
    JD.succeed MouseDown
