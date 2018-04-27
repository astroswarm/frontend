module ViewRunWebApplication exposing (RunningWebApplication)


type alias RunningWebApplication =
    { name : String
    , local_endpoint : String
    , slug : String
    }
