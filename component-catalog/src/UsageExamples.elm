module UsageExamples exposing (Msg, State, all)

import UsageExample exposing (UsageExample)
import UsageExamples.ClickableCardWithTooltip as ClickableCardWithTooltip


all : List (UsageExample State Msg)
all =
    [ ClickableCardWithTooltip.example
        |> UsageExample.wrapMsg ClickableCardWithTooltipMsg
            (\msg ->
                case msg of
                    ClickableCardWithTooltipMsg childMsg ->
                        Just childMsg
            )
        |> UsageExample.wrapState ClickableCardWithTooltipState
            (\msg ->
                case msg of
                    ClickableCardWithTooltipState childState ->
                        Just childState
            )
    ]


type State
    = ClickableCardWithTooltipState ClickableCardWithTooltip.State


type Msg
    = ClickableCardWithTooltipMsg ClickableCardWithTooltip.Msg
