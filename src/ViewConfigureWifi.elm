module ViewConfigureWifi exposing (viewModal)

import Bootstrap.Button
import Bootstrap.Form
import Bootstrap.Form.Input
import Bootstrap.Grid
import Bootstrap.Grid.Col
import Bootstrap.Modal
import Html
import Html.Attributes


viewModal :
    ( Bootstrap.Modal.State -> msg
    , { a
        | addWifiNetworkInFlight : Bool
        , removeWifiNetworkInFlight : Bool
        , removeWifiNetworkSelectedSsid : String
        , configureWifiModalState : Bootstrap.Modal.State
        , addWifiNetworkFormSsid : String
        , addWifiNetworkFormKey : String
        , wifiNetworks : List String
      }
    , msg
    , String -> msg
    , String -> msg
    , String -> msg
    )
    -> Html.Html msg
viewModal ( configure_wifi_modal_msg, model, add_wifi_network_msg, set_ssid_msg, set_key_msg, remove_wifi_network_msg ) =
    Bootstrap.Modal.config configure_wifi_modal_msg
        |> Bootstrap.Modal.large
        |> Bootstrap.Modal.h3 [] [ Html.text "Configure Wifi" ]
        |> Bootstrap.Modal.body []
            (List.append
                ([ Html.p [ Html.Attributes.hidden (List.length model.wifiNetworks > 0) ] [ Html.text "To enable wifi, specify one or more wireless networks." ]
                 , Bootstrap.Form.form []
                    [ Bootstrap.Form.group []
                        ([ Bootstrap.Grid.container
                            []
                            [ Bootstrap.Grid.row []
                                [ Bootstrap.Grid.col []
                                    [ Bootstrap.Form.Input.text
                                        [ Bootstrap.Form.Input.id "ssid"
                                        , Bootstrap.Form.Input.placeholder "SSID"
                                        , Bootstrap.Form.Input.value model.addWifiNetworkFormSsid
                                        , Bootstrap.Form.Input.onInput set_ssid_msg
                                        ]
                                    , Bootstrap.Form.help [] [ Html.text "The name of the network." ]
                                    ]
                                , Bootstrap.Grid.col []
                                    [ Bootstrap.Form.Input.text
                                        [ Bootstrap.Form.Input.id "security-key"
                                        , Bootstrap.Form.Input.placeholder "Security Key (None)"
                                        , Bootstrap.Form.Input.value model.addWifiNetworkFormKey
                                        , Bootstrap.Form.Input.onInput set_key_msg
                                        ]
                                    , Bootstrap.Form.help [] [ Html.text "Leave blank for open networks." ]
                                    ]
                                , Bootstrap.Grid.col []
                                    [ Bootstrap.Button.button
                                        [ Bootstrap.Button.primary
                                        , Bootstrap.Button.disabled (model.addWifiNetworkInFlight || model.addWifiNetworkFormSsid == "")
                                        , Bootstrap.Button.onClick (add_wifi_network_msg)
                                        ]
                                        [ Html.text
                                            (if model.addWifiNetworkInFlight then
                                                "Adding Network..."
                                             else
                                                "Add Network"
                                            )
                                        ]
                                    ]
                                ]
                            ]
                         ]
                        )
                    ]
                 ]
                )
                (if List.length model.wifiNetworks > 0 then
                    [ Html.p [] [ Html.text "Astrolab will connect to the first network it sees in the list below." ]
                    , Bootstrap.Grid.container []
                        (List.map
                            (\wifi_network ->
                                Bootstrap.Grid.row []
                                    [ Bootstrap.Grid.col [ Bootstrap.Grid.Col.xs3 ]
                                        [ Html.text (wifi_network)
                                        ]
                                    , Bootstrap.Grid.col [ Bootstrap.Grid.Col.xs2 ]
                                        [ Bootstrap.Button.button
                                            [ Bootstrap.Button.small
                                            , Bootstrap.Button.outlineDanger
                                            , Bootstrap.Button.disabled (model.removeWifiNetworkInFlight)
                                            , Bootstrap.Button.onClick (remove_wifi_network_msg wifi_network)
                                            ]
                                            [ Html.text
                                                (if
                                                    model.removeWifiNetworkInFlight
                                                        && model.removeWifiNetworkSelectedSsid
                                                        == wifi_network
                                                 then
                                                    "Removing..."
                                                 else
                                                    "Remove"
                                                )
                                            ]
                                        ]
                                    ]
                            )
                            model.wifiNetworks
                        )
                    ]
                 else
                    []
                )
            )
        |> Bootstrap.Modal.footer []
            [ Bootstrap.Button.button [ Bootstrap.Button.secondary, Bootstrap.Button.onClick (configure_wifi_modal_msg Bootstrap.Modal.hiddenState) ] [ Html.text "Close" ]
            ]
        |> Bootstrap.Modal.view model.configureWifiModalState
