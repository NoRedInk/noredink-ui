module Update exposing (Msg(..), subscriptions, update)

import Model exposing (..)
import NriModules as NriModules
import Routes as Routes exposing (Route)


type Msg
    = UpdateModuleStates NriModules.Msg
    | UrlChanged Route
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

        UrlChanged route ->
            ( { model | route = route }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UpdateModuleStates (NriModules.subscriptions model.moduleStates)
