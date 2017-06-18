module ViewUploadLogs exposing (viewModal)

import Bootstrap.Alert
import Bootstrap.Button
import Bootstrap.Modal
import Html
import Html.Attributes


viewModal :
    ( Bootstrap.Modal.State -> msg
    , { a
        | uploadLogsInFlight : Bool
        , uploadLogsModalState : Bootstrap.Modal.State
        , uploaded_log_url : String
      }
    , msg
    )
    -> Html.Html msg
viewModal ( upload_logs_modal_msg, model, upload_logs_msg ) =
    Bootstrap.Modal.config upload_logs_modal_msg
        |> Bootstrap.Modal.large
        |> Bootstrap.Modal.h3 [] [ Html.text "Upload Logs" ]
        |> Bootstrap.Modal.body []
            [ Html.p [] [ Html.text "If you're having trouble, we want to help!" ]
            , Html.p [] [ Html.text "The easiest way to diagnose your problem is for a developer to examine your system logs. These logs help us piece together a timeline of everything that has happened on your AstroSwarm computer." ]
            , Bootstrap.Alert.warning
                [ Html.p [] [ Html.text "Warning: these logs will be uploaded to a public web server where anybody can look at them. If you use AstroSwarm to handle sensitive data, please do not use this feature." ]
                ]
            , viewUploadLogsStatus model
            ]
        |> Bootstrap.Modal.footer []
            [ Bootstrap.Button.button
                [ Bootstrap.Button.primary
                , Bootstrap.Button.disabled (model.uploadLogsInFlight)
                , Bootstrap.Button.onClick (upload_logs_msg)
                ]
                [ Html.text
                    (if model.uploadLogsInFlight then
                        "Uploading..."
                     else
                        "Upload Logs"
                    )
                ]
            , Bootstrap.Button.button [ Bootstrap.Button.secondary, Bootstrap.Button.onClick (upload_logs_modal_msg Bootstrap.Modal.hiddenState) ] [ Html.text "Close" ]
            ]
        |> Bootstrap.Modal.view model.uploadLogsModalState


viewUploadLogsStatus : { a | uploaded_log_url : String } -> Html.Html msg
viewUploadLogsStatus model =
    Html.div []
        [ if String.length (model.uploaded_log_url) > 0 then
            Bootstrap.Alert.info
                [ Html.div []
                    [ Html.text "Your logs have been uploaded: "
                    , Html.a
                        [ Html.Attributes.href model.uploaded_log_url
                        , Html.Attributes.target "_blank"
                        ]
                        [ Html.text model.uploaded_log_url ]
                    ]
                ]
          else
            Html.text ""
        ]
