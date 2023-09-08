module UsageExamples exposing (Msg, State, all)

import UsageExample exposing (UsageExample)
import UsageExamples.ClickableCardWithTooltip as ClickableCardWithTooltip
import UsageExamples.Form as Form


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
    ]


type State
    = ClickableCardWithTooltipState ClickableCardWithTooltip.State
    | FormState Form.State


type Msg
    = ClickableCardWithTooltipMsg ClickableCardWithTooltip.Msg
    | FormMsg Form.Msg
