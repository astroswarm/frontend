module Configurator exposing (..)

import Navigation


determineApiHost : Navigation.Location -> String
determineApiHost location =
    case location.host of
        "localhost:3000" ->
            "localhost:3001"

        "app.astroswarm.com" ->
            "api.astroswarm.com"

        -- Err toward the production endpoint, in case of a config error in production
        _ ->
            "api.astroswarm.com"
