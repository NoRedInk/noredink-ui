module Model exposing (..)

import NriModules exposing (ModuleStates)
import Routes exposing (Route)


type alias Model =
    { -- Global UI
      route : Route
    , moduleStates : ModuleStates
    }
