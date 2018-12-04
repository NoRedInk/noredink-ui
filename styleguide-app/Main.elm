module Main exposing (init, main)

import Browser
import Browser.Navigation exposing (Key)
import Model exposing (..)
import NriModules as NriModules
import Routes as Routes exposing (Route(..))
import Update exposing (Msg(..), subscriptions, update)
import Url exposing (Url)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url _ =
    ( { route = Routes.fromLocation url
      , moduleStates = NriModules.init
      }
    , Cmd.none
    )
