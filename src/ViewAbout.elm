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
        , Html.p [] [ Html.text "After we build a community, we'll have a global network of telescopes. We'll use this network to make it easy to collaborate with other astronomers, and to share your findings with the larger world. People who don't have telescopes will be able to use and control them over the internet, and people who can't afford them will be able to participate for free." ]
        , Html.p [] [ Html.text "Over time, our platform will model how humans observe space, and allow the telescopes to look for things entirely on their own. Because they're networked, they can coordinate to make optimal use of hardware and geographic dispersion, both in search of new discoveries, and to provide around-the-clock coverage of known celestial events and phenomena." ]
        , Html.p [] [ Html.text "We hope you'll join us to make space study more accessible to everyone, and to learn more about our cosmos than we ever have before." ]
        ]
