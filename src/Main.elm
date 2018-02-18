-- Imports


module Main exposing (..)

import Bootstrap.Modal
import Bootstrap.Navbar
import Html
import Html.Attributes
import Http
import JsonApi
import Json.Decode
import Json.Encode
import Maybe
import Navigation
import Time exposing (Time)
import UrlParser exposing ((</>))
import AstrolabActivator
import Configurator
import ViewAbout
import ViewGettingStarted
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
    | UploadLogsRoute
    | ActivateAstrolabRoute
    | GettingStartedRoute
    | NotFoundRoute


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map RunApplicationRoute (UrlParser.s "run-application")
        , UrlParser.map ApplicationRoute (UrlParser.s "applications" </> UrlParser.string)
        , UrlParser.map WebApplicationRoute (UrlParser.s "webapplications" </> UrlParser.string)
        , UrlParser.map UploadLogsRoute (UrlParser.s "upload-logs")
        , UrlParser.map GettingStartedRoute (UrlParser.s "getting-started")
        , UrlParser.map ActivateAstrolabRoute (UrlParser.s "activate")
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



-- Constants


astrolabDetectionIntervalInSeconds : number
astrolabDetectionIntervalInSeconds =
    3


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
    , uploadLogsModalState : Bootstrap.Modal.State
    , uploadLogsInFlight : Bool
    , loadingAstrolabs : Bool
    , activatingAstrolab : Bool
    , route : Route
    , astrolabs : Maybe (List AstrolabActivator.Astrolab)
    , apiHost : String
    , selectedAstrolab : Maybe AstrolabActivator.Astrolab
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
          , uploadLogsModalState = Bootstrap.Modal.hiddenState
          , uploadLogsInFlight = False
          , loadingAstrolabs = False
          , activatingAstrolab = False
          , route = parseLocation location
          , astrolabs = Nothing
          , apiHost = Configurator.determineApiHost location
          , selectedAstrolab = Nothing
          }
        , navbarCmd
        )



-- Update


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
    | UploadLogsModalMsg Bootstrap.Modal.State
    | UploadLogs
      -- Astrolab-specific messages:
    | LoadAstrolabs
    | LoadAstrolabsComplete (Result Http.Error (List JsonApi.Resource))
    | SelectAstrolab (Maybe AstrolabActivator.Astrolab)
    | ActivateAstrolab
    | ActivateAstrolabComplete (Result Http.Error JsonApi.Resource)


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
            , Http.send HandleDetermineRunningApplications (determineRunningApplications model.selectedAstrolab)
            )

        HandleDetermineRunningApplications (Ok running_applications_list) ->
            ({ model | runningApplications = running_applications_list } |> update DetermineRunningWebApplications)

        HandleDetermineRunningApplications (Err err) ->
            ( model, Cmd.none )

        DetermineRunningWebApplications ->
            ( model
            , Http.send HandleDetermineRunningWebApplications (determineRunningWebApplications model.selectedAstrolab)
            )

        HandleDetermineRunningWebApplications (Ok running_web_applications_list) ->
            ( { model | runningWebApplications = running_web_applications_list }, Cmd.none )

        HandleDetermineRunningWebApplications (Err err) ->
            ( model, Cmd.none )

        StartApplication docker_image ->
            ( model
            , Http.send HandleStartApplication (startApplication model.selectedAstrolab docker_image)
            )

        HandleStartApplication (Ok response) ->
            ( model, Cmd.none )

        HandleStartApplication (Err err) ->
            ( model, Cmd.none )

        StopApplication docker_image ->
            ( model
            , Http.send HandleStopApplication (stopApplication model.selectedAstrolab docker_image)
            )

        HandleStopApplication (Ok response) ->
            ( model, Cmd.none )

        HandleStopApplication (Err err) ->
            ( model, Cmd.none )

        CleanApplication docker_image ->
            ( model
            , Http.send HandleCleanApplication (cleanApplication model.selectedAstrolab docker_image)
            )

        HandleCleanApplication (Ok response) ->
            ( model, Cmd.none )

        HandleCleanApplication (Err err) ->
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
            ( { model | loadingAstrolabs = True }, (AstrolabActivator.loadAstrolabs model LoadAstrolabsComplete) )

        LoadAstrolabsComplete (Ok resources) ->
            ( { model
                | loadingAstrolabs = False
                , astrolabs = AstrolabActivator.parseAstrolabs resources
              }
            , Cmd.none
            )

        LoadAstrolabsComplete (Err error) ->
            Debug.log ("Error: " ++ toString error)
                ( { model | loadingAstrolabs = False, astrolabs = Nothing }, Cmd.none )

        SelectAstrolab astrolab ->
            ({ model | selectedAstrolab = astrolab }
                |> update DetermineRunningApplications
            )

        ActivateAstrolab ->
            model |> update NoOp

        ActivateAstrolabComplete (Ok output) ->
            model |> update NoOp

        _ ->
            model |> update NoOp



-- DECODERS


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
            (case model.selectedAstrolab of
                Just astrolab ->
                    astrolab.local_endpoint ++ "/api/execute_command"

                Nothing ->
                    "http://localhost"
            )
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


determineRunningApplications : Maybe AstrolabActivator.Astrolab -> Http.Request (List ViewRunApplication.RunningApplication)
determineRunningApplications maybe_astrolab =
    Http.get
        (case maybe_astrolab of
            Just astrolab ->
                astrolab.local_endpoint ++ "/api/running_xapplications"

            Nothing ->
                "http://localhost"
        )
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


determineRunningWebApplications : Maybe AstrolabActivator.Astrolab -> Http.Request (List ViewRunWebApplication.RunningWebApplication)
determineRunningWebApplications maybe_astrolab =
    Http.get
        (case maybe_astrolab of
            Just astrolab ->
                astrolab.local_endpoint ++ "/api/running_webapplications"

            Nothing ->
                "http://localhost"
        )
        determineRunningWebApplicationsResponseDecoder


applicationSpecifierEncoder : String -> Json.Encode.Value
applicationSpecifierEncoder docker_image =
    Json.Encode.object [ ( "image", Json.Encode.string docker_image ) ]


applicationSpecifierResponseDecoder : Json.Decode.Decoder String
applicationSpecifierResponseDecoder =
    Json.Decode.field "status" Json.Decode.string


startApplication : Maybe AstrolabActivator.Astrolab -> String -> Http.Request String
startApplication maybe_astrolab docker_image =
    Http.post
        (case maybe_astrolab of
            Just astrolab ->
                astrolab.local_endpoint ++ "/api/start_xapplication"

            Nothing ->
                "http://localhost"
        )
        (Http.stringBody "application/json" <| Json.Encode.encode 0 <| applicationSpecifierEncoder docker_image)
        (applicationSpecifierResponseDecoder)


stopApplication : Maybe AstrolabActivator.Astrolab -> String -> Http.Request String
stopApplication maybe_astrolab docker_image =
    Http.post
        (case maybe_astrolab of
            Just astrolab ->
                astrolab.local_endpoint ++ "/api/stop_xapplication"

            Nothing ->
                "http://localhost"
        )
        (Http.stringBody "application/json" <| Json.Encode.encode 0 <| applicationSpecifierEncoder docker_image)
        (applicationSpecifierResponseDecoder)


cleanApplication : Maybe AstrolabActivator.Astrolab -> String -> Http.Request String
cleanApplication maybe_astrolab docker_image =
    Http.post
        (case maybe_astrolab of
            Just astrolab ->
                astrolab.local_endpoint ++ "/api/clean_xapplication"

            Nothing ->
                "http://localhost"
        )
        (Http.stringBody "application/json" <| Json.Encode.encode 0 <| applicationSpecifierEncoder docker_image)
        (applicationSpecifierResponseDecoder)



-- VIEW


vncIframeSrcForApplication : ViewRunApplication.RunningApplication -> String
vncIframeSrcForApplication app =
    "http://novnc.com/noVNC/vnc.html?autoconnect=true&reconnect=true&host="
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
                ActivateAstrolabRoute ->
                    AstrolabActivator.view ( model, SelectAstrolab )

                HomeRoute ->
                    ViewAbout.view

                GettingStartedRoute ->
                    ViewGettingStarted.view

                RunApplicationRoute ->
                    ViewRunApplication.view ( StartApplication, StopApplication, CleanApplication )

                ApplicationRoute running_application ->
                    (case (applicationFromName running_application model.runningApplications) of
                        Just application ->
                            Html.iframe
                                [ Html.Attributes.src
                                    (vncIframeSrcForApplication application)
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
            , ViewNavigation.view ( NavbarMsg, model, ApplicationSelect, WebApplicationSelect, UploadLogsModalMsg, LoadAstrolabs, SelectAstrolab )
            , ViewUploadLogs.viewModal ( UploadLogsModalMsg, model, UploadLogs )
            , viewServiceEmbed
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Bootstrap.Navbar.subscriptions model.navbarState NavbarMsg
        , (if model.route == ActivateAstrolabRoute then
            Time.every (astrolabDetectionIntervalInSeconds * Time.second) (always LoadAstrolabs)
           else
            Sub.none
          )
        , (if model.selectedAstrolab /= Nothing then
            Time.every (runningApplicationIntervalInSeconds * Time.second) (always DetermineRunningApplications)
           else
            Sub.none
          )
        ]


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = initialState
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
