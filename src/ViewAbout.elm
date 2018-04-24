module ViewAbout exposing (view)

import Html
import Html.Attributes


view : Html.Html msg
view =
    Html.div []
        [ Html.h1 [] [ Html.text "Welcome to Your Astrolab" ]
        , Html.p [] [ Html.text "Right now you can only communicate with your Astrolab over a Local Area Network. Internet-based control is coming soon, but I have been too busy with feature development to tend to the security concerns that internet-based controls require. I will launch internet-based control as soon as I can do so responsibly; I'm aiming for early June." ]
        , Html.p [] [ Html.text "The services running on your device are listed in the dropdown menu above. Select any service to control it via a web-based VNC session. All services remain running in the background when you are not viewing them." ]
        , Html.p []
            [ Html.text "Do you need more help? Please "
            , Html.a [ Html.Attributes.href "mailto:robby@freerobby.com" ] [ Html.text "email me" ]
            , Html.text " with any questions you have or difficulties you run into."
            ]
        , Html.p [] [ Html.text "In order to make your problems easier to debug, I've built a system to automatically upload all the logs from your Astrolab. Please find this tool in the dropdown menu above, and include a link to your logs in your email when seeking help." ]
        ]
