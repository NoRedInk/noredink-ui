module UsageExamples.PerformancePlayground exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Css
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V2 as UiIcon
import UsageExample exposing (UsageExample)


example : UsageExample State Msg
example =
    { name = "Performance Playground"
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , about =
        [ Text.mediumBody [ Text.plaintext "Page to check performance when rendering inline css" ]
        ]
    , categories = []
    }


view : State -> List (Html Msg)
view state =
    let
        currentValue =
            Control.currentValue state.config

        controlView : Int -> Html Msg
        controlView =
            -- We return lambda to avoid caseing on each item while we loop
            case currentValue.controlRendered of
                PlainButton ->
                    \i -> button [] [ text ("A button " ++ String.fromInt i) ]

                PlainText ->
                    \i -> span [] [ text ("A text " ++ String.fromInt i) ]

                Button ->
                    \i -> Button.button ("A button " ++ String.fromInt i) []

                ClickableText ->
                    \i -> ClickableText.button ("A button " ++ String.fromInt i) []

                ClickableSvg ->
                    \i -> ClickableSvg.button ("clickable-svg" ++ String.fromInt i) UiIcon.gear []

                Tooltip ->
                    \i ->
                        Tooltip.view
                            { id = "tooltip-" ++ String.fromInt i
                            , trigger =
                                \attributes ->
                                    ClickableText.button ("Text with tooltip " ++ String.fromInt i) []
                            }
                            [ Tooltip.plaintext "This is a tooltip."
                            , Tooltip.open (state.openTooltip == Just i)
                            , Tooltip.onToggle (ToggleTooltip i)
                            , Tooltip.onTop
                            , Tooltip.smallPadding
                            , Tooltip.fitToContent
                            ]
    in
    [ Control.view UpdateSettings state.config
    , Heading.h2
        [ Heading.plaintext "Playground"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , div []
        (List.repeat currentValue.controlCount ()
            |> List.indexedMap (\i _ -> controlView i)
        )
    ]


type ControlRendered
    = PlainButton
    | PlainText
    | Button
    | ClickableText
    | ClickableSvg
    | Tooltip


type alias Config =
    { controlRendered : ControlRendered
    , controlCount : Int
    }


{-| -}
type alias State =
    { config : Control Config
    , openTooltip : Maybe Int
    }


init : State
init =
    { config =
        Control.record Config
            |> Control.field "Control"
                (Control.choice
                    [ ( "Plain button", Control.value PlainButton )
                    , ( "Plain text", Control.value PlainText )
                    , ( "Nri.Ui.Button", Control.value Button )
                    , ( "Nri.Ui.ClickableText", Control.value ClickableText )
                    , ( "Nri.Ui.ClickableSvg", Control.value ClickableSvg )
                    , ( "Nri.Ui.Tooltip", Control.value Tooltip )
                    ]
                )
            |> Control.field "Count" (Control.int 1)
    , openTooltip = Nothing
    }


{-| -}
type Msg
    = UpdateSettings (Control Config)
    | ToggleTooltip Int Bool


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings newConfig ->
            ( { state | config = newConfig, openTooltip = Nothing }
            , Cmd.none
            )

        ToggleTooltip i isOpen ->
            if isOpen then
                ( { state | openTooltip = Just i }, Cmd.none )

            else
                ( { state | openTooltip = Nothing }, Cmd.none )
