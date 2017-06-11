-- Imports


module Main exposing (..)

import Bootstrap.CDN
import Bootstrap.Modal
import Bootstrap.Navbar
import Html
import Html.Attributes
import Http
import JsonApi
import JsonApi.Http
import JsonApi.Resources
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe
import Navigation
import UrlParser exposing ((</>))
import AstrolabActivator
import ViewAbout
import ViewNavigation
import ViewUploadLogs


-- Route


type Route
    = HomeRoute
    | ServiceRoute String
    | UploadLogsRoute
    | ActivateAstrolabRoute
    | NotFoundRoute


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map ServiceRoute (UrlParser.s "services" </> UrlParser.string)
        , UrlParser.map UploadLogsRoute (UrlParser.s "upload-logs")
        , UrlParser.map ActivateAstrolabRoute (UrlParser.s "activate")
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



-- Model


type alias Service =
    { name : String
    , websockify_port : Int
    }


type alias Model =
    { services : List Service
    , selected_service_name : String
    , uploaded_log_url : String
    , navbarState : Bootstrap.Navbar.State
    , uploadLogsModalState : Bootstrap.Modal.State
    , uploadLogsInFlight : Bool
    , loadingAstrolabs : Bool
    , activatingAstrolab : Bool
    , route : Route
    , astrolabs : Maybe (List AstrolabActivator.Astrolab)
    }



-- Model Initialization


initialState : Navigation.Location -> ( Model, Cmd Msg )
initialState location =
    let
        ( navbarState, navbarCmd ) =
            Bootstrap.Navbar.initialState NavbarMsg
    in
        ( { services =
                [ Service "Lin Guider (Autoguider)" 6101
                , Service "PHD2 (Autoguider)" 6102
                , Service "Open Sky Imager (Camera Controller)" 6103
                ]
          , selected_service_name = "Lin Guider (Autoguider)"
          , uploaded_log_url = ""
          , navbarState = navbarState
          , uploadLogsModalState = Bootstrap.Modal.hiddenState
          , uploadLogsInFlight = False
          , loadingAstrolabs = False
          , activatingAstrolab = False
          , route = parseLocation location
          , astrolabs = Nothing
          }
        , navbarCmd
        )



-- Update


type Msg
    = NoOp
    | OnLocationChange Navigation.Location
    | UpdateRoute Route
    | ServiceSelect String
    | LogsUploaded (Result Http.Error String)
    | NavbarMsg Bootstrap.Navbar.State
    | UploadLogsModalMsg Bootstrap.Modal.State
    | UploadLogs
      -- Astrolab-specific messages:
    | LoadAstrolabs
    | LoadAstrolabsComplete (Result Http.Error (List JsonApi.Resource))
    | ActivateAstrolab
    | ActivateAstrolabComplete (Result Http.Error JsonApi.Resource)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                { model | route = newRoute }
                    |> update (UpdateRoute newRoute)

        UpdateRoute newRoute ->
            case newRoute of
                ServiceRoute string ->
                    { model | route = newRoute }
                        |> update (ServiceSelect string)

                UploadLogsRoute ->
                    model
                        |> update (UploadLogsModalMsg Bootstrap.Modal.visibleState)

                _ ->
                    ( { model | route = newRoute }, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        UploadLogsModalMsg state ->
            ( { model | uploadLogsModalState = state }, Cmd.none )

        ServiceSelect new_service ->
            if new_service /= model.selected_service_name then
                { model | selected_service_name = new_service }
                    |> update (UpdateRoute (ServiceRoute new_service))
            else
                ( model, Cmd.none )

        UploadLogs ->
            ( { model | uploadLogsInFlight = True }, uploadLogs model )

        LogsUploaded (Ok output) ->
            let
                url =
                    output
                        |> String.filter (\c -> c /= '\n')
            in
                ( { model | uploaded_log_url = url, uploadLogsInFlight = False }, Cmd.none )

        LogsUploaded (Err error) ->
            ( { model | uploaded_log_url = (toString error), uploadLogsInFlight = False }, Cmd.none )

        LoadAstrolabs ->
            ( { model | loadingAstrolabs = True }, loadAstrolabs )

        LoadAstrolabsComplete (Ok resources) ->
            --            Debug.log (parseAstrolabs resources)
            ( { model
                | loadingAstrolabs = False
                , astrolabs = parseAstrolabs resources
              }
            , Cmd.none
            )

        LoadAstrolabsComplete (Err error) ->
            Debug.log ("Error: " ++ toString error)
                ( { model | loadingAstrolabs = False, astrolabs = Nothing }, Cmd.none )

        ActivateAstrolab ->
            model |> update NoOp

        ActivateAstrolabComplete (Ok output) ->
            model |> update NoOp

        _ ->
            model |> update NoOp



-- DECODERS


astrolabDecoder : Json.Decode.Decoder AstrolabActivator.Astrolab
astrolabDecoder =
    Json.Decode.Pipeline.decode AstrolabActivator.Astrolab
        |> Json.Decode.Pipeline.required "last-public-ip-address" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-seen-at" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-country-name" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-region-name" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-city" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-zip-code" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-time-zone" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-latitude" Json.Decode.float
        |> Json.Decode.Pipeline.required "last-longitude" Json.Decode.float


loadAstrolabs : Cmd Msg
loadAstrolabs =
    JsonApi.Http.getPrimaryResourceCollection "http://localhost:3000/v1/astrolabs"
        |> Http.send LoadAstrolabsComplete


parseAstrolabs astrolabs_list =
    List.map
        (\r ->
            (JsonApi.Resources.attributes astrolabDecoder r)
        )
        astrolabs_list
        |> listOfResultsToMaybeList


listOfResultsToMaybeList list =
    removeErrorFromList list
        |> Just


removeErrorFromList : List (Result a b) -> List b
removeErrorFromList list =
    case (List.reverse list) of
        (Ok a) :: xs ->
            a :: removeErrorFromList xs

        (Err b) :: xs ->
            Debug.log (toString b)
                removeErrorFromList
                xs

        [] ->
            []


logsUploadedDecoder : Json.Decode.Decoder String
logsUploadedDecoder =
    Json.Decode.field "output" Json.Decode.string


uploadLogs : Model -> Cmd Msg
uploadLogs model =
    let
        body =
            Json.Encode.object
                [ ( "command", Json.Encode.string "pastebinit" )
                , ( "args"
                  , Json.Encode.list
                        ([ Json.Encode.string "-b"
                         , Json.Encode.string "sprunge.us"
                         , Json.Encode.string "/mnt/host/var/log/syslog"
                         ]
                        )
                  )
                ]
                |> Http.jsonBody

        url =
            "http://localhost:8001/api/execute_command"
    in
        Http.post url body logsUploadedDecoder
            |> Http.send LogsUploaded



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        viewServiceEmbed =
            case model.route of
                ActivateAstrolabRoute ->
                    AstrolabActivator.view model

                HomeRoute ->
                    ViewAbout.view

                ServiceRoute service_name ->
                    Html.iframe
                        [ Html.Attributes.src
                            ("http://localhost:6080/vnc_auto.html?host=localhost&port="
                                ++ (List.filter (\n -> n.name == model.selected_service_name) model.services
                                        |> List.map .websockify_port
                                        |> List.head
                                        |> Maybe.withDefault 0
                                        |> toString
                                   )
                            )
                        , Html.Attributes.height 600
                        , Html.Attributes.width 1000
                        ]
                        []

                _ ->
                    Html.text "Nothing yet"
    in
        Html.div [ Html.Attributes.class "container" ]
            [ Bootstrap.CDN.stylesheet
            , ViewNavigation.view ( NavbarMsg, model, ServiceSelect, UploadLogsModalMsg, LoadAstrolabs )
            , ViewUploadLogs.viewModal ( UploadLogsModalMsg, model, UploadLogs )
            , viewServiceEmbed
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Bootstrap.Navbar.subscriptions model.navbarState NavbarMsg


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = initialState
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
