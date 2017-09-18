module ViewGettingStarted exposing (view)

import Html
import Html.Attributes


view : Html.Html msg
view =
    Html.div []
        [ Html.h1 [] [ Html.text "Getting Started with Your Astrolab" ]
        , Html.p [] [ Html.text "Thanks for participating in the AstroSwarm alpha!" ]
        , Html.p [] [ Html.text "Right now you can only communicate with your Astrolab over a Local Area Network, so please be sure you're on the same wifi network as your Astrolab. Internet-based control is coming very soon, but I have been too busy with feature development to tend to the security concerns that internet-based controls require. I will launch internet-based control as soon as I can do so responsibly; I'm aiming for early November." ]
        , Html.p [] [ Html.text "The services running on your device are listed in the dropdown menu above. Select any service you'd like to control to launch a web-based VNC session. All services remain running in the background (on isolated displays in isolated containers)." ]
        , Html.p []
            [ Html.text "Do you need more help? Please "
            , Html.a [ Html.Attributes.href "mailto:robby@freerobby.com" ] [ Html.text "email me" ]
            , Html.text " with any questions you have or difficulties you run into."
            ]
        , Html.p [] [ Html.text "In order to make your problems easier to debug, I've built a system to automatically upload all the logs from your Astrolab. Please find this tool in the dropdown menu above, and please include logs in your email when seeking help." ]
        ]
