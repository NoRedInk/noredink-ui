module TestApp exposing (app)

import App exposing (..)
import ProgramTest exposing (SimulatedEffect, createApplication)
import Routes
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import Url


app =
    createApplication
        { init = init
        , view = view
        , update = update
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start ()


simulateEffect : Effect -> SimulatedEffect Msg
simulateEffect effect =
    case effect of
        GoToRoute route ->
            SimulatedEffect.Navigation.pushUrl (Routes.toString route)

        GoToUrl url ->
            SimulatedEffect.Navigation.pushUrl (Url.toString url)

        Load loc ->
            SimulatedEffect.Navigation.load loc

        FocusOn id ->
            -- TODO: mock an implementation
            SimulatedEffect.Cmd.none

        None ->
            SimulatedEffect.Cmd.none

        Command cmd ->
            -- TODO: mock an implementation
            SimulatedEffect.Cmd.none
