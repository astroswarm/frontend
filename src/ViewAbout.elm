module ViewAbout exposing (view)

import Html
import Html.Attributes


view : Html.Html msg
view =
    Html.div []
        [ Html.h1 [] [ Html.text "Welcome to AstroSwarm!" ]
        , Html.p [] [ Html.text "We're making it easy for anyone to study space and share their findings." ]
        , Html.p []
            [ Html.text "First, we're making it easier for people who already have telescopes to use them and share their photos and data. Have a telescope? You should join us! "
            , Html.a [ Html.Attributes.href "mailto:robby@freerobby.com" ] [ Html.text "Email Robby" ]
            , Html.text " for details."
            ]
        , Html.p [] [ Html.text "After we build a community, we'll have a global network of telescopes. We'll use this network to make it easy to collaborate with other astronomers, and to share your findings with the larger world. People who don't have or can't afford telescopes will be able to use and control them for free over the internet." ]
        , Html.p [] [ Html.text "Over time, our platform will model how humans observe space, and then allow the telescopes to look for things entirely on their own. Because they're networked, they can coordinate with each other for around-the-clock coverage of interesting celestial phenomena, and we'll learn more about space than we ever have before." ]
        ]
