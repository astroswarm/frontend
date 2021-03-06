-- Imports


module Main exposing (..)

import Bootstrap.Modal
import Bootstrap.Navbar
import Html
import Html.Attributes
import Http
import Json.Decode
import Json.Encode
import Maybe
import Navigation
import Time exposing (Time)
import UrlParser exposing ((</>))
import Configurator
import ViewAbout
import ViewConfigureWifi
import ViewNavigation
import ViewRunApplication
import ViewRunWebApplication
import ViewUploadLogs


-- Route


type Route
    = HomeRoute
    | ApplicationRoute String
    | WebApplicationRoute String
    | RunApplicationRoute
    | ConfigureWifiRoute
    | UploadLogsRoute
    | NotFoundRoute


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map RunApplicationRoute (UrlParser.s "run-application")
        , UrlParser.map ApplicationRoute (UrlParser.s "applications" </> UrlParser.string)
        , UrlParser.map WebApplicationRoute (UrlParser.s "webapplications" </> UrlParser.string)
        , UrlParser.map ConfigureWifiRoute (UrlParser.s "configure-wifi")
        , UrlParser.map UploadLogsRoute (UrlParser.s "upload-logs")
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



-- Constants


runningApplicationIntervalInSeconds : number
runningApplicationIntervalInSeconds =
    2



-- Model


type alias Model =
    { lastLocation : Navigation.Location
    , selectedApplication : Maybe ViewRunApplication.RunningApplication
    , runningApplications : List ViewRunApplication.RunningApplication
    , selectedWebApplication : Maybe ViewRunWebApplication.RunningWebApplication
    , runningWebApplications : List ViewRunWebApplication.RunningWebApplication
    , uploaded_log_url : String
    , navbarState : Bootstrap.Navbar.State
    , configureWifiModalState : Bootstrap.Modal.State
    , addWifiNetworkInFlight : Bool
    , addWifiNetworkFormSsid : String
    , addWifiNetworkFormKey : String
    , removeWifiNetworkInFlight : Bool
    , removeWifiNetworkSelectedSsid : String
    , getWifiNetworksInFlight : Bool
    , wifiNetworks : List String
    , uploadLogsModalState : Bootstrap.Modal.State
    , uploadLogsInFlight : Bool
    , route : Route
    , brainApiHost : String
    }



-- Model Initialization


initialState : Navigation.Location -> ( Model, Cmd Msg )
initialState location =
    let
        ( navbarState, navbarCmd ) =
            Bootstrap.Navbar.initialState NavbarMsg
    in
        ( { lastLocation = location
          , selectedApplication = Nothing
          , runningApplications = []
          , selectedWebApplication = Nothing
          , runningWebApplications = []
          , uploaded_log_url = ""
          , navbarState = navbarState
          , configureWifiModalState = Bootstrap.Modal.hiddenState
          , addWifiNetworkInFlight = False
          , addWifiNetworkFormSsid = ""
          , addWifiNetworkFormKey = ""
          , removeWifiNetworkInFlight = False
          , removeWifiNetworkSelectedSsid = ""
          , getWifiNetworksInFlight = True
          , wifiNetworks = []
          , uploadLogsModalState = Bootstrap.Modal.hiddenState
          , uploadLogsInFlight = False
          , route = parseLocation location
          , brainApiHost = Configurator.determineBrainHost location
          }
        , navbarCmd
        )


type Msg
    = NoOp
    | OnLocationChange Navigation.Location
    | UpdateRoute Route
    | DetermineRunningApplications
    | HandleDetermineRunningApplications (Result Http.Error (List ViewRunApplication.RunningApplication))
    | DetermineRunningWebApplications
    | HandleDetermineRunningWebApplications (Result Http.Error (List ViewRunWebApplication.RunningWebApplication))
    | StartApplication String
    | HandleStartApplication (Result Http.Error String)
    | StopApplication String
    | HandleStopApplication (Result Http.Error String)
    | CleanApplication String
    | HandleCleanApplication (Result Http.Error String)
    | ApplicationSelect ViewRunApplication.RunningApplication
    | WebApplicationSelect ViewRunWebApplication.RunningWebApplication
    | LogsUploaded (Result Http.Error String)
    | NavbarMsg Bootstrap.Navbar.State
    | ConfigureWifiModalMsg Bootstrap.Modal.State
    | AddWifiNetwork
    | AddWifiNetworkSetFormSsid String
    | AddWifiNetworkSetFormKey String
    | WifiNetworkAdded (Result Http.Error String)
    | RemoveWifiNetwork String
    | WifiNetworkRemoved (Result Http.Error String)
    | GetWifiNetworks
    | WifiNetworksGotten (Result Http.Error (List String))
    | UploadLogs
    | UploadLogsModalMsg Bootstrap.Modal.State


applicationFromName : String -> List ViewRunApplication.RunningApplication -> Maybe ViewRunApplication.RunningApplication
applicationFromName name applications =
    List.head
        (List.filter
            (\app ->
                app.name == name
            )
            applications
        )


webApplicationFromSlug : String -> List ViewRunWebApplication.RunningWebApplication -> Maybe ViewRunWebApplication.RunningWebApplication
webApplicationFromSlug slug applications =
    List.head
        (List.filter
            (\app ->
                app.slug == slug
            )
            applications
        )


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
                { model | route = newRoute, lastLocation = location }
                    |> update (UpdateRoute newRoute)

        UpdateRoute newRoute ->
            if newRoute /= parseLocation model.lastLocation then
                case newRoute of
                    ApplicationRoute string ->
                        (case (applicationFromName string model.runningApplications) of
                            Just application ->
                                { model | route = newRoute }
                                    |> update (ApplicationSelect application)

                            Nothing ->
                                ( model, Cmd.none )
                        )

                    WebApplicationRoute string ->
                        (case (webApplicationFromSlug string model.runningWebApplications) of
                            Just application ->
                                { model | route = newRoute }
                                    |> update (WebApplicationSelect application)

                            Nothing ->
                                ( model, Cmd.none )
                        )

                    UploadLogsRoute ->
                        model
                            |> update (UploadLogsModalMsg Bootstrap.Modal.visibleState)

                    _ ->
                        ( { model | route = newRoute }, Cmd.none )
            else
                ( model, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        UploadLogsModalMsg state ->
            ( { model | uploadLogsModalState = state }, Cmd.none )

        ApplicationSelect running_application ->
            ({ model | selectedApplication = Just running_application }
                |> update (UpdateRoute (ApplicationRoute (running_application.name)))
            )

        WebApplicationSelect running_web_application ->
            ({ model | selectedWebApplication = Just running_web_application }
                |> update (UpdateRoute (WebApplicationRoute (running_web_application.name)))
            )

        DetermineRunningApplications ->
            ( model
            , Http.send HandleDetermineRunningApplications (determineRunningApplications model)
            )

        HandleDetermineRunningApplications (Ok running_applications_list) ->
            ({ model | runningApplications = running_applications_list } |> update DetermineRunningWebApplications)

        HandleDetermineRunningApplications (Err err) ->
            ( model, Cmd.none )

        DetermineRunningWebApplications ->
            ( model
            , Http.send HandleDetermineRunningWebApplications (determineRunningWebApplications model)
            )

        HandleDetermineRunningWebApplications (Ok running_web_applications_list) ->
            ( { model | runningWebApplications = running_web_applications_list }, Cmd.none )

        HandleDetermineRunningWebApplications (Err err) ->
            ( model, Cmd.none )

        StartApplication docker_image ->
            ( model
            , Http.send HandleStartApplication (startApplication model docker_image)
            )

        HandleStartApplication (Ok response) ->
            ( model, Cmd.none )

        HandleStartApplication (Err err) ->
            ( model, Cmd.none )

        StopApplication docker_image ->
            ( model
            , Http.send HandleStopApplication (stopApplication model docker_image)
            )

        HandleStopApplication (Ok response) ->
            ( model, Cmd.none )

        HandleStopApplication (Err err) ->
            ( model, Cmd.none )

        CleanApplication docker_image ->
            ( model
            , Http.send HandleCleanApplication (cleanApplication model docker_image)
            )

        HandleCleanApplication (Ok response) ->
            ( model, Cmd.none )

        HandleCleanApplication (Err err) ->
            ( model, Cmd.none )

        ------------------------
        -- Wifi Configuration --
        ------------------------
        ConfigureWifiModalMsg state ->
            if state == Bootstrap.Modal.visibleState then
                ({ model | configureWifiModalState = state } |> update GetWifiNetworks)
            else
                ( { model | configureWifiModalState = state }, Cmd.none )

        AddWifiNetwork ->
            ( { model | addWifiNetworkInFlight = True }, addWifiNetwork model )

        AddWifiNetworkSetFormSsid ssid ->
            ( { model | addWifiNetworkFormSsid = ssid }, Cmd.none )

        AddWifiNetworkSetFormKey key ->
            ( { model | addWifiNetworkFormKey = key }, Cmd.none )

        WifiNetworkAdded (Ok status) ->
            ({ model
                | addWifiNetworkInFlight = False
                , addWifiNetworkFormSsid = ""
                , addWifiNetworkFormKey = ""
             }
                |> update GetWifiNetworks
            )

        WifiNetworkAdded (Err error) ->
            ( { model | addWifiNetworkInFlight = False }, Cmd.none )

        -- Problem: model gets changed AFTER removeWifiModel; need to flip that order or pass ssid directly in
        RemoveWifiNetwork ssid ->
            ( { model
                | removeWifiNetworkSelectedSsid = ssid
                , removeWifiNetworkInFlight = True
              }
            , removeWifiNetwork model ssid
            )

        WifiNetworkRemoved (Ok status) ->
            ({ model
                | removeWifiNetworkInFlight = False
                , removeWifiNetworkSelectedSsid = ""
             }
                |> update GetWifiNetworks
            )

        WifiNetworkRemoved (Err error) ->
            ( { model
                | removeWifiNetworkInFlight = False
                , removeWifiNetworkSelectedSsid = ""
              }
            , Cmd.none
            )

        GetWifiNetworks ->
            ( { model | getWifiNetworksInFlight = True }, getWifiNetworks model )

        WifiNetworksGotten (Ok latest_wifi_networks) ->
            ( { model | wifiNetworks = latest_wifi_networks, getWifiNetworksInFlight = False }, Cmd.none )

        WifiNetworksGotten (Err error) ->
            ( { model | getWifiNetworksInFlight = False }, Cmd.none )

        -----------------
        -- Log Uploads --
        -----------------
        UploadLogs ->
            ( { model | uploadLogsInFlight = True }, uploadLogs model )

        LogsUploaded (Ok url) ->
            ( { model | uploaded_log_url = url, uploadLogsInFlight = False }, Cmd.none )

        LogsUploaded (Err error) ->
            ( { model | uploaded_log_url = (toString error), uploadLogsInFlight = False }, Cmd.none )



--        _ ->
--            model |> update NoOp
--
-- DECODERS
--


wifiNetworksGottenDecoder : Json.Decode.Decoder (List String)
wifiNetworksGottenDecoder =
    Json.Decode.list Json.Decode.string


wifiNetworkAddedDecoder : Json.Decode.Decoder String
wifiNetworkAddedDecoder =
    Json.Decode.field "status" Json.Decode.string


wifiNetworkRemovedDecoder : Json.Decode.Decoder String
wifiNetworkRemovedDecoder =
    Json.Decode.field "status" Json.Decode.string


addWifiNetwork : Model -> Cmd Msg
addWifiNetwork model =
    let
        body =
            Json.Encode.object
                [ ( "ssid", Json.Encode.string model.addWifiNetworkFormSsid )
                , ( "key", Json.Encode.string model.addWifiNetworkFormKey )
                ]
                |> Http.jsonBody

        url =
            (model.brainApiHost ++ "/api/add_wifi_network")
    in
        Http.post url body wifiNetworkAddedDecoder
            |> Http.send WifiNetworkAdded


removeWifiNetwork : Model -> String -> Cmd Msg
removeWifiNetwork model ssid =
    let
        body =
            Json.Encode.object
                [ ( "ssid", Json.Encode.string ssid ) ]
                |> Http.jsonBody

        url =
            (model.brainApiHost ++ "/api/remove_wifi_network")
    in
        Http.post url body wifiNetworkRemovedDecoder
            |> Http.send WifiNetworkRemoved


getWifiNetworks : Model -> Cmd Msg
getWifiNetworks model =
    let
        url =
            (model.brainApiHost ++ "/api/wifi_networks")
    in
        Http.get url wifiNetworksGottenDecoder
            |> Http.send WifiNetworksGotten


logsUploadedDecoder : Json.Decode.Decoder String
logsUploadedDecoder =
    Json.Decode.field "url" Json.Decode.string


uploadLogs : Model -> Cmd Msg
uploadLogs model =
    let
        body =
            Json.Encode.object
                []
                |> Http.jsonBody

        url =
            (model.brainApiHost ++ "/api/upload_logs")
    in
        Http.post url body logsUploadedDecoder
            |> Http.send LogsUploaded



-- APPLICATION HANDLING


runningApplicationDecoder : Json.Decode.Decoder ViewRunApplication.RunningApplication
runningApplicationDecoder =
    Json.Decode.map3 ViewRunApplication.RunningApplication
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "local_websockify_hostname" Json.Decode.string)
        (Json.Decode.field "local_websockify_port" Json.Decode.int)


determineRunningApplicationsResponseDecoder : Json.Decode.Decoder (List ViewRunApplication.RunningApplication)
determineRunningApplicationsResponseDecoder =
    Json.Decode.list runningApplicationDecoder


determineRunningApplications : Model -> Http.Request (List ViewRunApplication.RunningApplication)
determineRunningApplications model =
    Http.get
        (model.brainApiHost ++ "/api/running_xapplications")
        determineRunningApplicationsResponseDecoder


runningWebApplicationDecoder : Json.Decode.Decoder ViewRunWebApplication.RunningWebApplication
runningWebApplicationDecoder =
    Json.Decode.map3 ViewRunWebApplication.RunningWebApplication
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "local_endpoint" Json.Decode.string)
        (Json.Decode.field "slug" Json.Decode.string)


determineRunningWebApplicationsResponseDecoder : Json.Decode.Decoder (List ViewRunWebApplication.RunningWebApplication)
determineRunningWebApplicationsResponseDecoder =
    Json.Decode.list runningWebApplicationDecoder


determineRunningWebApplications : Model -> Http.Request (List ViewRunWebApplication.RunningWebApplication)
determineRunningWebApplications model =
    Http.get
        (model.brainApiHost ++ "/api/running_webapplications")
        determineRunningWebApplicationsResponseDecoder


applicationSpecifierEncoder : String -> Json.Encode.Value
applicationSpecifierEncoder docker_image =
    Json.Encode.object [ ( "image", Json.Encode.string docker_image ) ]


applicationSpecifierResponseDecoder : Json.Decode.Decoder String
applicationSpecifierResponseDecoder =
    Json.Decode.field "status" Json.Decode.string


startApplication : Model -> String -> Http.Request String
startApplication model docker_image =
    Http.post
        (model.brainApiHost ++ "/api/start_xapplication")
        (Http.stringBody "application/json" <| Json.Encode.encode 0 <| applicationSpecifierEncoder docker_image)
        (applicationSpecifierResponseDecoder)


stopApplication : Model -> String -> Http.Request String
stopApplication model docker_image =
    Http.post
        (model.brainApiHost ++ "/api/stop_xapplication")
        (Http.stringBody "application/json" <| Json.Encode.encode 0 <| applicationSpecifierEncoder docker_image)
        (applicationSpecifierResponseDecoder)


cleanApplication : Model -> String -> Http.Request String
cleanApplication model docker_image =
    Http.post
        (model.brainApiHost ++ "/api/clean_xapplication")
        (Http.stringBody "application/json" <| Json.Encode.encode 0 <| applicationSpecifierEncoder docker_image)
        (applicationSpecifierResponseDecoder)



-- VIEW


vncIframeSrcForApplication : Model -> ViewRunApplication.RunningApplication -> String
vncIframeSrcForApplication model app =
    model.brainApiHost
        ++ "/vendor/noVNC/vnc.html?autoconnect=true&reconnect=true&host="
        ++ app.local_websockify_hostname
        ++ "&port="
        ++ toString app.local_websockify_port


httpIframeSrcForWebApplication : ViewRunWebApplication.RunningWebApplication -> String
httpIframeSrcForWebApplication app =
    app.local_endpoint


view : Model -> Html.Html Msg
view model =
    let
        viewServiceEmbed =
            case model.route of
                HomeRoute ->
                    ViewAbout.view

                RunApplicationRoute ->
                    ViewRunApplication.view ( StartApplication, StopApplication, CleanApplication )

                ApplicationRoute running_application ->
                    (case (applicationFromName running_application model.runningApplications) of
                        Just application ->
                            Html.iframe
                                [ Html.Attributes.src
                                    (vncIframeSrcForApplication model application)
                                , Html.Attributes.height 600
                                , Html.Attributes.width 1000
                                ]
                                []

                        Nothing ->
                            Html.div [] []
                    )

                WebApplicationRoute running_web_application ->
                    (case (webApplicationFromSlug running_web_application model.runningWebApplications) of
                        Just application ->
                            Html.iframe
                                [ Html.Attributes.src
                                    (httpIframeSrcForWebApplication application)
                                , Html.Attributes.height 600
                                , Html.Attributes.width 1000
                                ]
                                []

                        Nothing ->
                            Html.div [] []
                    )

                _ ->
                    Html.text "Nothing yet"
    in
        Html.div [ Html.Attributes.class "container" ]
            [ Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "/bootstrap-4.0.0-alpha.6.min.css"
                ]
                []
            , Html.node "link"
                [ Html.Attributes.rel "stylesheet"
                , Html.Attributes.href "/font-awesome-4.7.0.min.css"
                ]
                []
            , ViewNavigation.view ( NavbarMsg, model, ApplicationSelect, WebApplicationSelect, ConfigureWifiModalMsg, UploadLogsModalMsg )
            , ViewConfigureWifi.viewModal ( ConfigureWifiModalMsg, model, AddWifiNetwork, AddWifiNetworkSetFormSsid, AddWifiNetworkSetFormKey, RemoveWifiNetwork )
            , ViewUploadLogs.viewModal ( UploadLogsModalMsg, model, UploadLogs )
            , viewServiceEmbed
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Bootstrap.Navbar.subscriptions model.navbarState NavbarMsg
        , Time.every (runningApplicationIntervalInSeconds * Time.second) (always DetermineRunningApplications)
        , Time.every (runningApplicationIntervalInSeconds * Time.second) (always DetermineRunningWebApplications)
        ]


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = initialState
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
