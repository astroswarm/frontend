module AstrolabActivator exposing (view)

import Bootstrap.Table
import Html
import Html.Attributes


view =
    Html.div []
        [ Html.p [] [ Html.text "Plug your Astrolab into your router and turn it on. Wait 30 seconds, and you should see it below." ]
        , Bootstrap.Table.table
            { options = [ Bootstrap.Table.hover, Bootstrap.Table.small ]
            , thead =
                Bootstrap.Table.simpleThead
                    [ Bootstrap.Table.th [] [ Html.text "Public IP" ]
                    , Bootstrap.Table.th [] [ Html.text "Last Detected" ]
                    , Bootstrap.Table.th [] [ Html.text "Country" ]
                    , Bootstrap.Table.th [] [ Html.text "Region" ]
                    , Bootstrap.Table.th [] [ Html.text "City" ]
                    , Bootstrap.Table.th [] [ Html.text "Zip Code" ]
                    , Bootstrap.Table.th [] [ Html.text "Latitude" ]
                    , Bootstrap.Table.th [] [ Html.text "Longitude" ]
                    ]
            , tbody =
                Bootstrap.Table.tbody []
                    [ Bootstrap.Table.tr []
                        [ Bootstrap.Table.td [] [ Html.text "147.148.156.100" ]
                        , Bootstrap.Table.td [] [ Html.text "2017-06-11T04:26:12.706Z" ]
                        , Bootstrap.Table.td [] [ Html.text "United Kingdom" ]
                        , Bootstrap.Table.td [] [ Html.text "England" ]
                        , Bootstrap.Table.td [] [ Html.text "Chichester" ]
                        , Bootstrap.Table.td [] [ Html.text "PO20" ]
                        , Bootstrap.Table.td [] [ Html.text "50.8383" ]
                        , Bootstrap.Table.td [] [ Html.text "-0.6708" ]
                        ]
                    , Bootstrap.Table.tr []
                        [ Bootstrap.Table.td [] [ Html.text "86.101.75.9" ]
                        , Bootstrap.Table.td [] [ Html.text "2017-06-10T04:26:12.706Z" ]
                        , Bootstrap.Table.td [] [ Html.text "United States" ]
                        , Bootstrap.Table.td [] [ Html.text "Massachusetts" ]
                        , Bootstrap.Table.td [] [ Html.text "Boston" ]
                        , Bootstrap.Table.td [] [ Html.text "02110" ]
                        , Bootstrap.Table.td [] [ Html.text "47.8383" ]
                        , Bootstrap.Table.td [] [ Html.text "-9.6708" ]
                        ]
                    , Bootstrap.Table.tr []
                        [ Bootstrap.Table.td [] [ Html.text "23.57.94.4" ]
                        , Bootstrap.Table.td [] [ Html.text "2017-06-10T04:26:12.706Z" ]
                        , Bootstrap.Table.td [] [ Html.text "United States" ]
                        , Bootstrap.Table.td [] [ Html.text "Massachusetts" ]
                        , Bootstrap.Table.td [] [ Html.text "Arlington" ]
                        , Bootstrap.Table.td [] [ Html.text "02133" ]
                        , Bootstrap.Table.td [] [ Html.text "47.8391" ]
                        , Bootstrap.Table.td [] [ Html.text "-9.6798" ]
                        ]
                    ]
            }
        ]
