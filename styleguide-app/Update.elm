module Update exposing (Msg(..), subscriptions, update)

import Browser
import Model exposing (..)
import NriModules as NriModules
import Url exposing (Url)


type Msg
    = UpdateModuleStates NriModules.Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        UpdateModuleStates msg ->
            let
                ( moduleStates, cmd ) =
                    NriModules.update msg model.moduleStates
            in
            ( { model | moduleStates = moduleStates }
            , Cmd.map UpdateModuleStates cmd
            )

        OnUrlRequest request ->
            -- TODO
            ( model, Cmd.none )

        OnUrlChange route ->
            -- TODO
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateModuleStates (NriModules.subscriptions model.moduleStates)
