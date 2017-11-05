module ViewRunWebApplication exposing (RunningWebApplication)

import Html
import Html.Attributes
import Html.Events


type alias RunningWebApplication =
    { name : String
    , local_endpoint : String
    , slug : String
    }
