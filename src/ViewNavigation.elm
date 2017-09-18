module ViewNavigation exposing (view)

import Bootstrap.Modal
import Bootstrap.Navbar
import Html
import Html.Attributes
import Html.Events
import AstrolabActivator


view :
    ( Bootstrap.Navbar.State -> msg
    , { b
        | navbarState : Bootstrap.Navbar.State
        , selected_service_name : Maybe String
        , services : List { a | name : String }
        , selectedAstrolab : Maybe AstrolabActivator.Astrolab
      }
    , Maybe String -> msg
    , Bootstrap.Modal.State -> msg
    , msg
    , Maybe AstrolabActivator.Astrolab -> msg
    )
    -> Html.Html msg
view ( navbar_msg, model, service_select_cmd, upload_logs_modal_msg, load_astrolabs_msg, select_astrolab_msg ) =
    Html.div []
        [ Bootstrap.Navbar.config navbar_msg
            |> Bootstrap.Navbar.withAnimation
            |> Bootstrap.Navbar.brand [ Html.Attributes.href "#" ] [ Html.text "AstroSwarm" ]
            |> Bootstrap.Navbar.items
                [ Bootstrap.Navbar.itemLink
                    [ Html.Attributes.href "#" ]
                    [ Html.text "About" ]
                , (case model.selectedAstrolab of
                    Nothing ->
                        Bootstrap.Navbar.itemLink
                            [ Html.Attributes.href "#activate"
                            , Html.Events.onClick (load_astrolabs_msg)
                            ]
                            [ Html.text "Select Your Astrolab" ]

                    Just astrolab ->
                        Bootstrap.Navbar.dropdown
                            { id = "serviceSelect"
                            , toggle = Bootstrap.Navbar.dropdownToggle [] [ Html.text "Select Option" ]
                            , items =
                                List.concat
                                    [ (List.map
                                        (\service ->
                                            Bootstrap.Navbar.dropdownItem
                                                [ Html.Events.onClick (service_select_cmd (Just service.name))
                                                ]
                                                [ Html.text service.name ]
                                        )
                                        model.services
                                      )
                                    , ([ Bootstrap.Navbar.dropdownDivider
                                       , Bootstrap.Navbar.dropdownItem
                                            [ Html.Events.onClick (upload_logs_modal_msg Bootstrap.Modal.visibleState)
                                            ]
                                            [ Html.text "Get Help (Upload Logs)" ]
                                       ]
                                      )
                                    , ([ Bootstrap.Navbar.dropdownDivider
                                       , Bootstrap.Navbar.dropdownItem
                                            [ Html.Attributes.href "#activate"
                                            , Html.Events.onClick (select_astrolab_msg Nothing)
                                            ]
                                            [ Html.text "Select a Different Astrolab" ]
                                       ]
                                      )
                                    ]
                            }
                  )
                ]
            |> Bootstrap.Navbar.view model.navbarState
        ]
