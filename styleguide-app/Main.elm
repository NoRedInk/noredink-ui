module Main exposing (..)

import Model exposing (..)
import Navigation
import NriModules as NriModules
import Routes as Routes exposing (Route(..))
import Update exposing (Msg(..), subscriptions, update)
import View exposing (view)


main : Program Never Model Msg
main =
    Navigation.program
        (Routes.fromLocation >> UrlChanged)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { route = Routes.fromLocation location
      , moduleStates = NriModules.init
      }
    , Cmd.none
    )
