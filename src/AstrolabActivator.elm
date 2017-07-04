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
    { public_ip_address : String
    , private_ip_address : String
    , last_seen_at : String
    , country_name : String
    , region_name : String
    , city : String
    , zip_code : String
    , time_zone : String
    , latitude : Float
    , longitude : Float
    }


loadAstrolabs : { a | apiHost : String } -> (Result Http.Error (List JsonApi.Resource) -> msg) -> Cmd msg
loadAstrolabs model load_astrolabs_complete_msg =
    JsonApi.Http.getPrimaryResourceCollection ("http://" ++ model.apiHost ++ "/v1/astrolabs")
        |> Http.send load_astrolabs_complete_msg


astrolabDecoder : Json.Decode.Decoder Astrolab
astrolabDecoder =
    Json.Decode.Pipeline.decode Astrolab
        |> Json.Decode.Pipeline.required "public-ip-address" Json.Decode.string
        |> Json.Decode.Pipeline.required "private-ip-address" Json.Decode.string
        |> Json.Decode.Pipeline.required "last-seen-at" Json.Decode.string
        |> Json.Decode.Pipeline.required "country-name" Json.Decode.string
        |> Json.Decode.Pipeline.required "region-name" Json.Decode.string
        |> Json.Decode.Pipeline.required "city" Json.Decode.string
        |> Json.Decode.Pipeline.required "zip-code" Json.Decode.string
        |> Json.Decode.Pipeline.required "time-zone" Json.Decode.string
        |> Json.Decode.Pipeline.required "latitude" Json.Decode.float
        |> Json.Decode.Pipeline.required "longitude" Json.Decode.float


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
                                        [ Bootstrap.Table.td [] [ Html.text astrolab.public_ip_address ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.private_ip_address ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.last_seen_at ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.country_name ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.region_name ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.city ]
                                        , Bootstrap.Table.td [] [ Html.text astrolab.zip_code ]
                                        , Bootstrap.Table.td [] [ Html.text (toString astrolab.latitude) ]
                                        , Bootstrap.Table.td [] [ Html.text (toString astrolab.longitude) ]
                                        ]
                                )
                                astrolabs
                            )
            }
        ]
