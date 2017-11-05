module ViewNavigation exposing (view)

import Bootstrap.Modal
import Bootstrap.Navbar
import Html
import Html.Attributes
import Html.Events
import AstrolabActivator
import ViewRunApplication
import ViewRunWebApplication


view :
    ( Bootstrap.Navbar.State -> msg
    , { b
        | navbarState : Bootstrap.Navbar.State
        , runningApplications : List ViewRunApplication.RunningApplication
        , runningWebApplications : List ViewRunWebApplication.RunningWebApplication
        , selectedAstrolab : Maybe AstrolabActivator.Astrolab
      }
    , ViewRunApplication.RunningApplication -> msg
    , ViewRunWebApplication.RunningWebApplication -> msg
    , Bootstrap.Modal.State -> msg
    , msg
    , Maybe AstrolabActivator.Astrolab -> msg
    )
    -> Html.Html msg
view ( navbar_msg, model, application_select_msg, web_application_select_msg, upload_logs_modal_msg, load_astrolabs_msg, select_astrolab_msg ) =
    Html.div []
        [ Bootstrap.Navbar.config navbar_msg
            |> Bootstrap.Navbar.withAnimation
            |> Bootstrap.Navbar.brand [ Html.Attributes.href "#" ] [ Html.text "AstroSwarm" ]
            |> Bootstrap.Navbar.items
                [ (case model.selectedAstrolab of
                    Nothing ->
                        Bootstrap.Navbar.itemLink
                            [ Html.Attributes.href "#activate"
                            , Html.Events.onClick (load_astrolabs_msg)
                            ]
                            [ Html.text "Select Your Astrolab" ]

                    Just astrolab ->
                        Bootstrap.Navbar.dropdown
                            { id = "serviceSelect"
                            , toggle =
                                Bootstrap.Navbar.dropdownToggle []
                                    [ Html.text ("Your Astrolab: " ++ astrolab.local_endpoint)
                                    ]
                            , items =
                                List.concat
                                    [ ([ Bootstrap.Navbar.dropdownItem
                                            [ Html.Attributes.href "#getting-started"
                                            ]
                                            [ Html.text "Getting Started" ]
                                       ]
                                      )
                                    , ([ Bootstrap.Navbar.dropdownDivider
                                       , Bootstrap.Navbar.dropdownItem
                                            [ Html.Attributes.href "#run-application"
                                            ]
                                            [ Html.text "Run an application..." ]
                                       ]
                                      )
                                    , (List.map
                                        (\application ->
                                            Bootstrap.Navbar.dropdownItem
                                                [ Html.Attributes.href ("#applications/" ++ application.name)
                                                ]
                                                [ Html.text application.name ]
                                        )
                                        model.runningApplications
                                      )
                                    , ([ Bootstrap.Navbar.dropdownDivider
                                       , Bootstrap.Navbar.dropdownItem
                                            [ Html.Events.onClick (upload_logs_modal_msg Bootstrap.Modal.visibleState)
                                            ]
                                            [ Html.text "Get Help (Upload Logs)" ]
                                       ]
                                      )
                                    , ([ Bootstrap.Navbar.dropdownDivider
                                       , Bootstrap.Navbar.dropdownHeader [ Html.text "Developer Tools" ]
                                       ]
                                      )
                                    , (List.map
                                        (\application ->
                                            Bootstrap.Navbar.dropdownItem
                                                [ Html.Attributes.href ("#webapplications/" ++ application.slug)
                                                ]
                                                [ Html.text application.name ]
                                        )
                                        model.runningWebApplications
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
