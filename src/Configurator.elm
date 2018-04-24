module Configurator exposing (..)

import Navigation


determineBrainHost : Navigation.Location -> String
determineBrainHost location =
    case location.host of
        _ ->
            "http://" ++ location.hostname ++ ":5003"
