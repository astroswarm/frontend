module AstrolabActivator exposing (Astrolab, loadAstrolabs, parseAstrolabs, view)

import Bootstrap.Table
import Html
import Http
import Json.Decode
import Json.Decode.Pipeline
import JsonApi
import JsonApi.Http
import JsonApi.Resources


type alias Astrolab =
    { last_public_ip_address : String
    , last_private_ip_address : String
    , last_seen_at : String
    , last_country_name : String
    , last_region_name : String
    , last_city : String
    , last_zip_code : String
    , last_time_zone : String
    , last_latitude : Float
    , last_longitude : Float
    }


loadAstrolabs : (Result Http.Error (List JsonApi.Resource) -> msg) -> Cmd msg
loadAstrolabs load_astrolabs_complete_msg =
    JsonApi.Http.getPrimaryResourceCollection "http://localhost:3001/v1/astrolabs"
        |> Http.send load_astrolabs_complete_msg


astrolabDecoder : Json.Decode.Decoder Astrolab
astrolabDecoder =
    Json.Decode.Pipeline.decode Astrolab
        |> Json.Decode.Pipeline.required "last-public-ip-address" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-private-ip-address" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-seen-at" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-country-name" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-region-name" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-city" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-zip-code" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-time-zone" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-latitude" Json.Decode.float
        |> Json.Decode.Pipeline.required "last-longitude" Json.Decode.float


parseAstrolabs : List JsonApi.Resource -> Maybe (List Astrolab)
parseAstrolabs astrolabs_list =
    List.map
        (\r ->
            (JsonApi.Resources.attributes astrolabDecoder r)
        )
        astrolabs_list
        |> listOfResultsToMaybeList



-- Consider using Maybe-Extra in replace of the following two functions.
-- https://github.com/elm-community/maybe-extra/blob/4.0.0/src/Maybe/Extra.elm


listOfResultsToMaybeList : List (Result a b) -> Maybe (List b)
listOfResultsToMaybeList list =
    removeErrorFromList list
        |> Just


removeErrorFromList : List (Result a b) -> List b
removeErrorFromList list =
    case (List.reverse list) of
        (Ok a) :: xs ->
            a :: removeErrorFromList xs

        (Err b) :: xs ->
            Debug.log (toString b)
                removeErrorFromList
                xs

        [] ->
            []


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
                    , Bootstrap.Table.th [] [ Html.text "Private IP" ]
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
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_private_ip_address ]
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
