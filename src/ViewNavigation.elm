module ViewNavigation exposing (view)

import Bootstrap.Modal
import Bootstrap.Navbar
import Html
import Html.Attributes
import Html.Events


view :
    ( Bootstrap.Navbar.State -> msg
    , { b
        | navbarState : Bootstrap.Navbar.State
        , selected_service_name : String
        , services : List { a | name : String }
      }
    , String -> msg
    , Bootstrap.Modal.State -> msg
    , msg
    )
    -> Html.Html msg
view ( navbar_msg, model, service_select_cmd, upload_logs_modal_msg, load_astrolabs_msg ) =
    Html.div []
        [ Bootstrap.Navbar.config navbar_msg
            |> Bootstrap.Navbar.withAnimation
            |> Bootstrap.Navbar.brand [ Html.Attributes.href "#" ] [ Html.text "AstroSwarm" ]
            |> Bootstrap.Navbar.items
                [ Bootstrap.Navbar.itemLink
                    [ Html.Attributes.href "#" ]
                    [ Html.text "About" ]
                , Bootstrap.Navbar.itemLink
                    [ Html.Attributes.href "#activate"
                    , Html.Events.onClick (load_astrolabs_msg)
                    ]
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
                                        Bootstrap.Navbar.dropdownItem [ Html.Events.onClick (service_select_cmd service.name) ] [ Html.text service.name ]
                                )
                        )
                    }
                , Bootstrap.Navbar.dropdown
                    { id = "getHelp"
                    , toggle = Bootstrap.Navbar.dropdownToggle [] [ Html.text "Get Help" ]
                    , items =
                        [ Bootstrap.Navbar.dropdownItem
                            [ Html.Events.onClick (upload_logs_modal_msg Bootstrap.Modal.visibleState)
                            ]
                            [ Html.text "Upload Logs" ]
                        ]
                    }
                ]
            |> Bootstrap.Navbar.view model.navbarState
        ]
