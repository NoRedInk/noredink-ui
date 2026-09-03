module Examples.HintTooltip exposing (Msg, State, example)

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.HintTooltip.V1 as HintTooltip
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "HintTooltip"


version : Int
version =
    1


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ previewStage
            [ HintTooltip.view
                { id = "hint-tooltip-preview"
                , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-preview-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Hover target" ]
                }
                [ HintTooltip.plaintext "Escapes clipping."
                , HintTooltip.smallPadding
                , HintTooltip.exactWidth 140
                , HintTooltip.auxiliaryDescription
                , HintTooltip.open True
                ]
            ]
        ]
    , about =
        [ Text.mediumBody [ Text.plaintext "Use HintTooltip.V1 for non-interactive hover/focus descriptions that should escape overflow clipping. Keep Tooltip.V3 when you still need the legacy API surface." ]
        ]
    , view = view
    , categories = [ Messaging ]
    , keyboardSupport =
        [ { keys = [ Esc ]
          , result = "While focusing the trigger, closes the tooltip through onToggle False."
          }
        ]
    }


type alias State =
    { clippingDemoOpen : Bool
    , scrollDemoOpen : Bool
    , primaryLabelOpen : Bool
    , auxiliaryOpen : Bool
    , flipOpen : Bool
    , edgeOpen : Bool
    }


init : State
init =
    { clippingDemoOpen = False
    , scrollDemoOpen = False
    , primaryLabelOpen = False
    , auxiliaryOpen = False
    , flipOpen = False
    , edgeOpen = False
    }


type Msg
    = ToggleClippingDemo Bool
    | ToggleScrollDemo Bool
    | TogglePrimaryLabel Bool
    | ToggleAuxiliary Bool
    | ToggleFlip Bool
    | ToggleEdge Bool


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleClippingDemo isOpen ->
            ( { state | clippingDemoOpen = isOpen }, Cmd.none )

        ToggleScrollDemo isOpen ->
            ( { state | scrollDemoOpen = isOpen }, Cmd.none )

        TogglePrimaryLabel isOpen ->
            ( { state | primaryLabelOpen = isOpen }, Cmd.none )

        ToggleAuxiliary isOpen ->
            ( { state | auxiliaryOpen = isOpen }, Cmd.none )

        ToggleFlip isOpen ->
            ( { state | flipOpen = isOpen }, Cmd.none )

        ToggleEdge isOpen ->
            ( { state | edgeOpen = isOpen }, Cmd.none )


view : ellieLinkConfig -> State -> List (Html Msg)
view _ state =
    [ Heading.h2 [ Heading.plaintext "Use this instead of Tooltip.V3 for simple descriptions" ]
    , Text.mediumBody
        [ Text.plaintext "HintTooltip.V1 is for plain descriptive content only. It keeps the legacy controlled API shape, but the overlay renders through the anchored overlay runtime so it is not clipped by overflow hidden/auto ancestors."
        ]
    , Heading.h2 [ Heading.plaintext "Clipping demo" ]
    , clippedStage
        [ HintTooltip.view
            { id = "hint-tooltip-demo"
            , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Focus or hover me" ]
            }
            [ HintTooltip.plaintext "This tooltip should stay visible even though the blue panel clips its contents."
            , HintTooltip.auxiliaryDescription
            , HintTooltip.fitToContent
            , HintTooltip.onToggle ToggleClippingDemo
            , HintTooltip.open state.clippingDemoOpen
            ]
        ]
    , scrollStage
        [ HintTooltip.view
            { id = "hint-tooltip-scroll"
            , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-scroll-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Scroll container trigger" ]
            }
            [ HintTooltip.plaintext "This tooltip should also escape an overflow auto container."
            , HintTooltip.auxiliaryDescription
            , HintTooltip.fitToContent
            , HintTooltip.onToggle ToggleScrollDemo
            , HintTooltip.open state.scrollDemoOpen
            ]
        ]
    , Heading.h2 [ Heading.plaintext "Primary label behavior" ]
    , Html.div
        [ css
            [ Css.displayFlex
            , Css.property "gap" "24px"
            , Css.flexWrap Css.wrap
            , Css.marginBottom (Css.px 72)
            ]
        ]
        [ HintTooltip.view
            { id = "hint-tooltip-primary-label"
            , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-primary-label-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Print" ]
            }
            [ HintTooltip.plaintext "Print"
            , HintTooltip.primaryLabel
            , HintTooltip.smallPadding
            , HintTooltip.fitToContent
            , HintTooltip.onToggle TogglePrimaryLabel
            , HintTooltip.open state.primaryLabelOpen
            ]
        , HintTooltip.view
            { id = "hint-tooltip-auxiliary"
            , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-auxiliary-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Save draft" ]
            }
            [ HintTooltip.plaintext "Preview mode only"
            , HintTooltip.auxiliaryDescription
            , HintTooltip.smallPadding
            , HintTooltip.fitToContent
            , HintTooltip.onToggle ToggleAuxiliary
            , HintTooltip.open state.auxiliaryOpen
            ]
        ]
    , Heading.h2 [ Heading.plaintext "Viewport fitting" ]
    , Html.div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.flexStart
            , Css.width (Css.pct 100)
            , Css.property "gap" "24px"
            , Css.marginTop (Css.px 20)
            ]
        ]
        [ HintTooltip.view
            { id = "hint-tooltip-flip"
            , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-flip-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Flip near viewport top" ]
            }
            [ HintTooltip.plaintext "When you scroll this trigger to the top of the viewport, the tooltip should flip below it."
            , HintTooltip.auxiliaryDescription
            , HintTooltip.onToggle ToggleFlip
            , HintTooltip.open state.flipOpen
            ]
        , HintTooltip.view
            { id = "hint-tooltip-edge"
            , trigger = \attrs -> Html.button (Attributes.id "hint-tooltip-edge-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Shift near edge" ]
            }
            [ HintTooltip.plaintext "This tooltip prefers the right side, but should shift back into the viewport."
            , HintTooltip.auxiliaryDescription
            , HintTooltip.onRight
            , HintTooltip.onToggle ToggleEdge
            , HintTooltip.open state.edgeOpen
            ]
        ]
    ]


clippedStage : List (Html msg) -> Html msg
clippedStage children =
    Html.div
        [ Attributes.id "hint-tooltip-clipping-demo"
        , css
            [ Css.marginTop (Css.px 20)
            , Css.width (Css.px 320)
            , Css.height (Css.px 130)
            , Css.padding (Css.px 16)
            , Css.borderRadius (Css.px 16)
            , Css.backgroundColor (Css.rgb 232 244 255)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 160 204 240)
            , Css.overflow Css.hidden
            ]
        ]
        [ Html.div
            [ css
                [ Css.height (Css.px 220)
                , Css.displayFlex
                , Css.alignItems Css.flexStart
                , Css.justifyContent Css.flexStart
                , Css.paddingTop (Css.px 4)
                ]
            ]
            children
        ]


scrollStage : List (Html msg) -> Html msg
scrollStage children =
    Html.div
        [ Attributes.id "hint-tooltip-scroll-demo"
        , css
            [ Css.marginTop (Css.px 20)
            , Css.width (Css.px 320)
            , Css.height (Css.px 130)
            , Css.padding (Css.px 16)
            , Css.borderRadius (Css.px 16)
            , Css.backgroundColor (Css.rgb 232 244 255)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 160 204 240)
            , Css.overflow Css.auto
            ]
        ]
        [ Html.div
            [ css
                [ Css.height (Css.px 220)
                , Css.displayFlex
                , Css.alignItems Css.flexStart
                , Css.justifyContent Css.flexStart
                , Css.paddingTop (Css.px 4)
                ]
            ]
            children
        ]


previewStage : List (Html msg) -> Html msg
previewStage children =
    Html.div
        [ css
            [ Css.marginTop (Css.px 12)
            , Css.width (Css.pct 100)
            , Css.height (Css.px 96)
            , Css.padding (Css.px 12)
            , Css.boxSizing Css.borderBox
            , Css.borderRadius (Css.px 16)
            , Css.backgroundColor (Css.rgb 232 244 255)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 160 204 240)
            , Css.overflow Css.hidden
            ]
        ]
        [ Html.div
            [ css
                [ Css.height (Css.pct 100)
                , Css.displayFlex
                , Css.alignItems Css.flexStart
                , Css.justifyContent Css.flexStart
                , Css.paddingTop (Css.px 4)
                ]
            ]
            children
        ]
