module Examples.Tooltip exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled.Attributes as Attributes exposing (css, href)
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V2 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon


example : Example State Msg
example =
    { name = "Tooltip"
    , version = 2
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Html.div
            [ css
                [ Css.marginTop (Css.px 60)
                , Css.alignSelf Css.center
                ]
            ]
            [ Tooltip.view
                { id = "preview-tooltip"
                , trigger =
                    \attributes ->
                        ClickableSvg.button "example-preview-tooltip-icon"
                            UiIcon.gear
                            [ ClickableSvg.custom attributes
                            , ClickableSvg.small
                            , ClickableSvg.custom [ Key.tabbable False ]
                            ]
                }
                [ Tooltip.plaintext "This is a tooltip."
                , Tooltip.open True
                , Tooltip.onTop
                , Tooltip.smallPadding
                , Tooltip.fitToContent
                ]
            ]
        ]
    , view = view
    }


type alias State =
    { openTooltip : Maybe TooltipType
    , staticExampleSettings : Control (List (Tooltip.Attribute Never))
    }


init : State
init =
    { openTooltip = Nothing
    , staticExampleSettings = initStaticExampleSettings
    }


type TooltipType
    = PrimaryLabel
    | AuxillaryDescription
    | LearnMore


type Msg
    = ToggleTooltip TooltipType Bool
    | SetControl (Control (List (Tooltip.Attribute Never)))


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ToggleTooltip type_ isOpen ->
            if isOpen then
                ( { model | openTooltip = Just type_ }, Cmd.none )

            else
                ( { model | openTooltip = Nothing }, Cmd.none )

        SetControl settings ->
            ( { model | staticExampleSettings = settings }, Cmd.none )


view : State -> List (Html Msg)
view model =
    [ Heading.h3 [] [ Html.text "Using the Tooltip module" ]
    , Text.mediumBody
        [ Text.html
            [ Html.text "Label the Tooltip as either being the "
            , viewPrimaryLabelTooltip model.openTooltip
            , Html.text " or the "
            , viewAuxillaryDescriptionToolip model.openTooltip
            , Html.text " for the trigger content."
            , viewToggleTip model.openTooltip
            ]
        ]
    , viewCustomizableExample model.staticExampleSettings
    ]


viewPrimaryLabelTooltip : Maybe TooltipType -> Html Msg
viewPrimaryLabelTooltip openTooltip =
    Tooltip.view
        { id = "tooltip__primaryLabel"
        , trigger =
            \eventHandlers ->
                ClickableText.button "primaryLabel"
                    [ ClickableText.custom eventHandlers
                    ]
        }
        [ Tooltip.html
            [ Html.text "A primary label is used when the tooltip content serves as the main label for its trigger content"
            , Html.br []
            , Html.text "e.g. when the trigger content is an icon with no text."
            ]
        , Tooltip.auxillaryDescription
        , Tooltip.onHover (ToggleTooltip PrimaryLabel)
        , Tooltip.open (openTooltip == Just PrimaryLabel)
        , Tooltip.onBottom
        ]


viewAuxillaryDescriptionToolip : Maybe TooltipType -> Html Msg
viewAuxillaryDescriptionToolip openTooltip =
    Tooltip.view
        { id = "tooltip__auxillaryDescription"
        , trigger =
            \eventHandlers ->
                ClickableText.button "auxillaryDescription"
                    [ ClickableText.custom eventHandlers
                    ]
        }
        [ Tooltip.html
            [ Html.text "An auxillary description is used when the tooltip content provides supplementary information about its trigger content"
            , Html.br []
            , Html.text "e.g. when the trigger content is a word in the middle of a body of text that requires additional explanation."
            ]
        , Tooltip.auxillaryDescription
        , Tooltip.onHover (ToggleTooltip AuxillaryDescription)
        , Tooltip.open (openTooltip == Just AuxillaryDescription)
        , Tooltip.onBottom
        ]


viewToggleTip : Maybe TooltipType -> Html Msg
viewToggleTip openTooltip =
    Tooltip.toggleTip { label = "tooltip__learn-more" }
        [ Tooltip.html
            [ Html.a
                [ href "https://inclusive-components.design/tooltips-toggletips" ]
                [ Html.text "Learn more" ]
            ]
        , Tooltip.primaryLabel
        , Tooltip.onHover (ToggleTooltip LearnMore)
        , Tooltip.open (openTooltip == Just LearnMore)
        , Tooltip.smallPadding
        , Tooltip.fitToContent
        ]


initStaticExampleSettings : Control (List (Tooltip.Attribute Never))
initStaticExampleSettings =
    ControlExtra.list
        |> ControlExtra.listItem "content" controlContent
        |> ControlExtra.optionalListItem "direction" controlDirection
        |> ControlExtra.optionalListItem "alignment" controlAlignment
        |> ControlExtra.optionalBoolListItem "withoutTail" Tooltip.withoutTail
        |> ControlExtra.optionalListItem "width" controlWidth
        |> ControlExtra.optionalListItem "padding" controlPadding


controlContent : Control (Tooltip.Attribute Never)
controlContent =
    CommonControls.content
        { moduleName = "Tooltip"
        , plaintext = Tooltip.plaintext
        , markdown = Nothing
        , html = Tooltip.html
        , httpError = Nothing
        }
        |> Control.map Tuple.second


controlDirection : Control (Tooltip.Attribute Never)
controlDirection =
    Control.choice
        [ ( "onTop", Control.value Tooltip.onTop )
        , ( "onBottom", Control.value Tooltip.onBottom )
        , ( "onLeft", Control.value Tooltip.onLeft )
        , ( "onRight", Control.value Tooltip.onRight )
        ]


controlAlignment : Control (Tooltip.Attribute Never)
controlAlignment =
    Control.choice
        [ ( "alignMiddle (default)", Control.value Tooltip.alignMiddle )
        , ( "alignStart", Control.map (Css.px >> Tooltip.alignStart) controlNumber )
        , ( "alignEnd", Control.map (Css.px >> Tooltip.alignEnd) controlNumber )
        ]


controlNumber : Control Float
controlNumber =
    Control.map (String.toFloat >> Maybe.withDefault 0) (Control.string "0")


controlWidth : Control (Tooltip.Attribute Never)
controlWidth =
    Control.choice
        [ ( "exactWidth 320 (default)", Control.value (Tooltip.exactWidth 320) )
        , ( "exactWidth", Control.map (round >> Tooltip.exactWidth) controlNumber )
        , ( "fitToContent", Control.value Tooltip.fitToContent )
        ]


controlPadding : Control (Tooltip.Attribute Never)
controlPadding =
    Control.choice
        [ ( "normalPadding (default)", Control.value Tooltip.normalPadding )
        , ( "smallPadding", Control.value Tooltip.smallPadding )
        , ( "customPadding", Control.map Tooltip.customPadding controlNumber )
        ]


viewCustomizableExample : Control (List (Tooltip.Attribute Never)) -> Html Msg
viewCustomizableExample controlSettings =
    let
        settings =
            Control.currentValue controlSettings

        attributes =
            Tooltip.open True :: settings
    in
    Html.div []
        [ ControlView.view
            { update = SetControl
            , settings = controlSettings
            , toExampleCode =
                \controls ->
                    [ { sectionName = "Example"
                      , code =
                            String.join "\n"
                                [ "Tooltip.view"
                                , "    { trigger ="
                                , "        \\popupTriggerAttributes ->"
                                , "            ClickableSvg.button \"Up\""
                                , "                UiIcon.arrowTop"
                                , "                [ ClickableSvg.custom popupTriggerAttributes"
                                , "                ]"
                                , "    , id = \"an-id-for-the-tooltip\""
                                , "    }"
                                , "    [ "
                                    ++ String.join "\n    , "
                                        ("Tooltip.open True" :: List.map (\_ -> "TODO") controls)
                                , "    ]"
                                ]
                      }
                    ]
            }
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.height (Css.px 300)
                ]
            ]
            [ Tooltip.view
                { trigger =
                    \eventHandlers ->
                        ClickableSvg.button "Up"
                            UiIcon.arrowTop
                            [ ClickableSvg.custom eventHandlers
                            ]
                , id = "an-id-for-the-tooltip"
                }
                attributes
                |> Html.map never
            ]
        ]
