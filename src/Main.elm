-- Imports


module Main exposing (..)

import Bootstrap.Alert
import Bootstrap.Button
import Bootstrap.CDN
import Bootstrap.Modal
import Bootstrap.Navbar
import Bootstrap.Table
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Maybe
import Navigation
import UrlParser exposing ((</>))
import ViewAbout


-- Route


type Route
    = HomeRoute
    | ServiceRoute String
    | UploadLogsRoute
    | ActivateAstrolabRoute
    | NotFoundRoute



-- These following parsers are provided by the url-parser library


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
    , route : Route
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
          , route = parseLocation location
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
            "http://localhost:8001/api/execute_command"
    in
        Http.post url body logsUploadedDecoder
            |> Http.send LogsUploaded



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        viewNavbar =
            Html.div []
                [ Bootstrap.Navbar.config NavbarMsg
                    |> Bootstrap.Navbar.withAnimation
                    |> Bootstrap.Navbar.brand [ Html.Attributes.href "#" ] [ Html.text "AstroSwarm" ]
                    |> Bootstrap.Navbar.items
                        [ Bootstrap.Navbar.itemLink
                            [ Html.Attributes.href "#" ]
                            [ Html.text "About" ]
                        , Bootstrap.Navbar.itemLink
                            [ Html.Attributes.href "#activate" ]
                            [ Html.text "Activate Your Astrolab" ]
                        , Bootstrap.Navbar.dropdown
                            { id = "serviceSelect"
                            , toggle = Bootstrap.Navbar.dropdownToggle [] [ Html.text model.selected_service_name ]
                            , items =
                                (List.filter (\service -> service.name /= model.selected_service_name) model.services
                                    |> List.map
                                        (\service ->
                                            if service.name == model.selected_service_name then
                                                Bootstrap.Navbar.dropdownItem [] [ Html.text service.name ]
                                            else
                                                Bootstrap.Navbar.dropdownItem [ Html.Events.onClick (ServiceSelect service.name) ] [ Html.text service.name ]
                                        )
                                )
                            }
                        , Bootstrap.Navbar.dropdown
                            { id = "getHelp"
                            , toggle = Bootstrap.Navbar.dropdownToggle [] [ Html.text "Get Help" ]
                            , items =
                                [ Bootstrap.Navbar.dropdownItem
                                    [ Html.Events.onClick (UploadLogsModalMsg Bootstrap.Modal.visibleState)
                                    ]
                                    [ Html.text "Upload Logs" ]
                                ]
                            }
                        ]
                    |> Bootstrap.Navbar.view model.navbarState
                ]

        viewUploadLogs =
            Html.div []
                [ if String.length (model.uploaded_log_url) > 0 then
                    Bootstrap.Alert.info
                        [ Html.div []
                            [ Html.text "Your logs have been uploaded: "
                            , Html.a
                                [ Html.Attributes.href model.uploaded_log_url
                                , Html.Attributes.target "_blank"
                                ]
                                [ Html.text model.uploaded_log_url ]
                            ]
                        ]
                  else
                    Html.text ""
                ]

        viewActivatableAstrolabs =
            Html.div []
                [ Html.p [] [ Html.text "Plug your Astrolab into your router and turn it on. Wait 30 seconds, and you should see it below." ]
                , Bootstrap.Table.table
                    { options = [ Bootstrap.Table.hover, Bootstrap.Table.small ]
                    , thead =
                        Bootstrap.Table.simpleThead
                            [ Bootstrap.Table.th [] [ Html.text "Public IP" ]
                            , Bootstrap.Table.th [] [ Html.text "Last Detected" ]
                            , Bootstrap.Table.th [] [ Html.text "Country" ]
                            , Bootstrap.Table.th [] [ Html.text "Region" ]
                            , Bootstrap.Table.th [] [ Html.text "City" ]
                            , Bootstrap.Table.th [] [ Html.text "Zip Code" ]
                            , Bootstrap.Table.th [] [ Html.text "Latitude" ]
                            , Bootstrap.Table.th [] [ Html.text "Longitude" ]
                            ]
                    , tbody =
                        Bootstrap.Table.tbody []
                            [ Bootstrap.Table.tr []
                                [ Bootstrap.Table.td [] [ Html.text "147.148.156.100" ]
                                , Bootstrap.Table.td [] [ Html.text "2017-06-11T04:26:12.706Z" ]
                                , Bootstrap.Table.td [] [ Html.text "United Kingdom" ]
                                , Bootstrap.Table.td [] [ Html.text "England" ]
                                , Bootstrap.Table.td [] [ Html.text "Chichester" ]
                                , Bootstrap.Table.td [] [ Html.text "PO20" ]
                                , Bootstrap.Table.td [] [ Html.text "50.8383" ]
                                , Bootstrap.Table.td [] [ Html.text "-0.6708" ]
                                ]
                            , Bootstrap.Table.tr []
                                [ Bootstrap.Table.td [] [ Html.text "86.101.75.9" ]
                                , Bootstrap.Table.td [] [ Html.text "2017-06-10T04:26:12.706Z" ]
                                , Bootstrap.Table.td [] [ Html.text "United States" ]
                                , Bootstrap.Table.td [] [ Html.text "Massachusetts" ]
                                , Bootstrap.Table.td [] [ Html.text "Boston" ]
                                , Bootstrap.Table.td [] [ Html.text "02110" ]
                                , Bootstrap.Table.td [] [ Html.text "47.8383" ]
                                , Bootstrap.Table.td [] [ Html.text "-9.6708" ]
                                ]
                            , Bootstrap.Table.tr []
                                [ Bootstrap.Table.td [] [ Html.text "23.57.94.4" ]
                                , Bootstrap.Table.td [] [ Html.text "2017-06-10T04:26:12.706Z" ]
                                , Bootstrap.Table.td [] [ Html.text "United States" ]
                                , Bootstrap.Table.td [] [ Html.text "Massachusetts" ]
                                , Bootstrap.Table.td [] [ Html.text "Arlington" ]
                                , Bootstrap.Table.td [] [ Html.text "02133" ]
                                , Bootstrap.Table.td [] [ Html.text "47.8391" ]
                                , Bootstrap.Table.td [] [ Html.text "-9.6798" ]
                                ]
                            ]
                    }
                ]

        viewUploadLogsModal =
            Bootstrap.Modal.config UploadLogsModalMsg
                |> Bootstrap.Modal.large
                |> Bootstrap.Modal.h3 [] [ Html.text "Upload Logs" ]
                |> Bootstrap.Modal.body []
                    [ Html.p [] [ Html.text "If you're having trouble, we want to help!" ]
                    , Html.p [] [ Html.text "The easiest way to diagnose your problem is for a developer to examine your system logs. These logs help us piece together a timeline of everything that has happened on your AstroSwarm computer." ]
                    , Bootstrap.Alert.warning
                        [ Html.p [] [ Html.text "Warning: these logs will be uploaded to a public web server where anybody can look at them. If you use AstroSwarm to handle sensitive data, please do not use this feature." ]
                        ]
                    , viewUploadLogs
                    ]
                |> Bootstrap.Modal.footer []
                    [ Bootstrap.Button.button
                        [ Bootstrap.Button.primary
                        , Bootstrap.Button.disabled (model.uploadLogsInFlight)
                        , Bootstrap.Button.onClick (UploadLogs)
                        ]
                        [ Html.text
                            (if model.uploadLogsInFlight then
                                "Uploading..."
                             else
                                "Upload Logs"
                            )
                        ]
                    , Bootstrap.Button.button [ Bootstrap.Button.secondary, Bootstrap.Button.onClick (UploadLogsModalMsg Bootstrap.Modal.hiddenState) ] [ Html.text "Close" ]
                    ]
                |> Bootstrap.Modal.view model.uploadLogsModalState

        viewServiceEmbed =
            case model.route of
                ActivateAstrolabRoute ->
                    viewActivatableAstrolabs

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
            , viewNavbar
            , viewUploadLogsModal
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