module ViewAbout exposing (view)

import Html
import Html.Attributes


view : Html.Html msg
view =
    Html.div []
        [ Html.h1 [] [ Html.text "Welcome to Your Astrolab" ]
        , Html.p [] [ Html.text "The applications running on your Astrolab are listed in the menu above. Select any application to begin using it. Applications run in the background whenever your device is on, so it's safe to close your browser tab." ]
        , Html.p [] [ Html.text "Astrolab runs on a Local Area Network. Internet-based control is coming to public beta later this year." ]
        , Html.h5 [] [ Html.text "Need Help?" ]
        , Html.p []
            [ Html.text "Please email me at "
            , Html.a [ Html.Attributes.href "mailto:robby@freerobby.com" ] [ Html.text "robby@freerobby.com" ]
            , Html.text " with any questions or difficulties."
            ]
        , Html.p [] [ Html.text "To make problems easier to debug, your Astrolab includes a tool to upload all of its logs for analysis. Please find this tool in the dropdown menu above, and include a link to your logs in your email when seeking help." ]
        ]
