module ViewRunApplication exposing (view, RunningApplication)

import Html
import Html.Attributes
import Html.Events


type alias ApplicationTemplate =
    { name : String
    , docker_image : String
    }


type alias RunningApplication =
    { name : String
    , local_websockify_hostname : String
    , local_websockify_port : Int
    }


applicationTemplates : List ApplicationTemplate
applicationTemplates =
    [ ApplicationTemplate "PHD Guiding" "astroswarm/phd2-x86_64:latest"
    , ApplicationTemplate "Open Sky Imager" "astroswarm/openskyimager-x86_64:latest"
    ]


view : ( String -> msg, String -> msg, String -> msg ) -> Html.Html msg
view ( start_application_msg, stop_application_msg, clean_application_msg ) =
    Html.div []
        [ Html.h1 [] [ Html.text "Run an Application..." ]
        , Html.p []
            [ Html.text "Any ARM-compatible Linux application can be made to run on your Astrolab! "
            , Html.a
                [ Html.Attributes.href "https://github.com/astroswarm/phd2_builder"
                , Html.Attributes.target "_blank"
                ]
                [ Html.text "Click here" ]
            , Html.text " for an example of how to do this."
            ]
        , Html.p [] [ Html.text "For now, we recommend using our supported applications. Click on a template below to get started. The first time you do this, the application will be downloaded, installed, and then run. It will not need to be redownloaded or reinstalled on subsequent runs." ]
        , Html.h2 [] [ Html.text "Available Applications" ]
        , Html.ul []
            (List.map
                (\application ->
                    Html.li []
                        [ Html.text (application.name ++ ": ")
                        , Html.a
                            [ Html.Attributes.href "#run-application"
                            , Html.Events.onClick (start_application_msg application.docker_image)
                            ]
                            [ Html.text "Start" ]
                        , Html.text " - "
                        , Html.a
                            [ Html.Attributes.href "#run-application"
                            , Html.Events.onClick (stop_application_msg application.docker_image)
                            ]
                            [ Html.text "Stop" ]
                        , Html.text " - "
                        , Html.a
                            [ Html.Attributes.href "#run-application"
                            , Html.Events.onClick (clean_application_msg application.docker_image)
                            ]
                            [ Html.text "Clean" ]
                        ]
                )
                applicationTemplates
            )
        ]
