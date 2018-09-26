module Model exposing (Model)

import NriModules exposing (ModuleStates)
import Routes exposing (Route)


type alias Model =
    { -- Global UI
      route : Route
    , moduleStates : ModuleStates
    }
