module UsageExamples exposing (Msg, State, all)

import UsageExample exposing (UsageExample)
import UsageExamples.ClickableCardWithTooltip as ClickableCardWithTooltip
import UsageExamples.FocusLoop as FocusLoop
import UsageExamples.Form as Form
import UsageExamples.MediaQuery as MediaQuery


all : List (UsageExample State Msg)
all =
    [ ClickableCardWithTooltip.example
        |> UsageExample.wrapMsg ClickableCardWithTooltipMsg
            (\msg ->
                case msg of
                    ClickableCardWithTooltipMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> UsageExample.wrapState ClickableCardWithTooltipState
            (\msg ->
                case msg of
                    ClickableCardWithTooltipState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , Form.example
        |> UsageExample.wrapMsg FormMsg
            (\msg ->
                case msg of
                    FormMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> UsageExample.wrapState FormState
            (\msg ->
                case msg of
                    FormState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , FocusLoop.example
        |> UsageExample.wrapMsg FocusLoopMsg
            (\msg ->
                case msg of
                    FocusLoopMsg childMsg ->
                        Just childMsg

                    _ ->
                        Nothing
            )
        |> UsageExample.wrapState FocusLoopState
            (\msg ->
                case msg of
                    FocusLoopState childState ->
                        Just childState

                    _ ->
                        Nothing
            )
    , MediaQuery.example
        |> UsageExample.noop NoOp
        |> UsageExample.stateless Stateless
    ]


type State
    = ClickableCardWithTooltipState ClickableCardWithTooltip.State
    | FormState Form.State
    | FocusLoopState FocusLoop.State
    | Stateless


type Msg
    = ClickableCardWithTooltipMsg ClickableCardWithTooltip.Msg
    | FormMsg Form.Msg
    | FocusLoopMsg FocusLoop.Msg
    | NoOp
