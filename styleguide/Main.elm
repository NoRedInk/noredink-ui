module Main exposing (main)

import App exposing (..)
import Browser
import Browser.Navigation as Navigation


main : Program () (Model Navigation.Key) Msg
main =
    Browser.application
        { init =
            \() flags key ->
                let
                    ( model, effect ) =
                        init () flags key
                in
                ( model, perform model.navigationKey effect )
        , update =
            \msg oldModel ->
                let
                    ( model, effect ) =
                        update msg oldModel
                in
                ( model, perform model.navigationKey effect )
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
