module Model exposing (Model)

import Browser.Navigation exposing (Key)
import NriModules exposing (ModuleStates)
import Routes exposing (Route)


type alias Model =
    { -- Global UI
      route : Route
    , moduleStates : ModuleStates
    , navigationKey : Key
    }
