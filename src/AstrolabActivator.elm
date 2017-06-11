module AstrolabActivator exposing (Astrolab, view)

import Bootstrap.Table
import Html


type alias Astrolab =
    { last_public_ip_address : String
    , last_seen_at : String
    , last_country_name : String
    , last_region_name : String
    , last_city : String
    , last_zip_code : String
    , last_time_zone : String
    , last_latitude : Float
    , last_longitude : Float
    }


view : { model | astrolabs : Maybe (List Astrolab), loadingAstrolabs : Bool } -> Html.Html msg
view model =
    Html.div []
        [ Html.p []
            [ Html.text
                (if model.loadingAstrolabs then
                    "Loading unregisted Astrolabs..."
                 else
                    "Loaded."
                )
            ]
        , Html.p [] [ Html.text "Plug your Astrolab into your router and turn it on. Wait 30 seconds, and you should see it below." ]
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
                case model.astrolabs of
                    Nothing ->
                        Bootstrap.Table.tbody [] [ Bootstrap.Table.tr [] [] ]

                    Just astrolabs ->
                        Bootstrap.Table.tbody []
                            (List.map
                                (\astrolab ->
                                    Bootstrap.Table.tr []
                                        [ Bootstrap.Table.td [] [ Html.text astrolab.last_public_ip_address ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_seen_at ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_country_name ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_region_name ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_city ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_zip_code ]
                                        , Bootstrap.Table.td [] [ Html.text (toString astrolab.last_latitude) ]
                                        , Bootstrap.Table.td [] [ Html.text (toString astrolab.last_longitude) ]
                                        ]
                                )
                                astrolabs
                            )
            }
        ]
