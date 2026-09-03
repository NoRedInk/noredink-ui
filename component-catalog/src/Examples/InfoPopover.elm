module Examples.InfoPopover exposing (Msg, State, example)

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.InfoPopover.V1 as InfoPopover
import Nri.Ui.Text.V6 as Text


moduleName : String
moduleName =
    "InfoPopover"


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
            [ InfoPopover.view
                { id = "info-popover-preview"
                , triggerId = "info-popover-preview-trigger"
                , label = "Planet details"
                , trigger = \attrs -> Html.button (Attributes.id "info-popover-preview-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Details" ]
                }
                [ InfoPopover.html
                    [ Html.p [ css [ Css.margin Css.zero ] ] [ Html.text "Rich content escapes clipping." ]
                    ]
                , InfoPopover.smallPadding
                , InfoPopover.exactWidth 150
                , InfoPopover.open True
                ]
            ]
        ]
    , about =
        [ Text.mediumBody [ Text.plaintext "Use InfoPopover.V1 for click/tap disclosures and rich content. This is the overflow-safe successor for the disclosure-style tooltip use cases that do not fit HintTooltip.V1." ]
        ]
    , view = view
    , categories = [ Messaging ]
    , keyboardSupport =
        [ { keys = [ Esc ]
          , result = "When the popover is open, Escape dispatches onToggle False."
          }
        ]
    }


type alias State =
    { clippingDemoOpen : Bool
    , scrollDemoOpen : Bool
    , rightAlignedOpen : Bool
    , mobileFallbackOpen : Bool
    , flipOpen : Bool
    , edgeOpen : Bool
    }


init : State
init =
    { clippingDemoOpen = False
    , scrollDemoOpen = False
    , rightAlignedOpen = False
    , mobileFallbackOpen = False
    , flipOpen = False
    , edgeOpen = False
    }


type Msg
    = ToggleClippingDemo Bool
    | ToggleScrollDemo Bool
    | ToggleRightAligned Bool
    | ToggleMobileFallback Bool
    | ToggleFlip Bool
    | ToggleEdge Bool


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleClippingDemo isOpen ->
            ( { state | clippingDemoOpen = isOpen }, Cmd.none )

        ToggleScrollDemo isOpen ->
            ( { state | scrollDemoOpen = isOpen }, Cmd.none )

        ToggleRightAligned isOpen ->
            ( { state | rightAlignedOpen = isOpen }, Cmd.none )

        ToggleMobileFallback isOpen ->
            ( { state | mobileFallbackOpen = isOpen }, Cmd.none )

        ToggleFlip isOpen ->
            ( { state | flipOpen = isOpen }, Cmd.none )

        ToggleEdge isOpen ->
            ( { state | edgeOpen = isOpen }, Cmd.none )


view : ellieLinkConfig -> State -> List (Html Msg)
view _ state =
    [ Heading.h2 [ Heading.plaintext "Use this instead of disclosure-style Tooltip.V3" ]
    , Text.mediumBody
        [ Text.plaintext "InfoPopover.V1 is the interactive/rich-content path. It uses the same anchored overlay runtime as HintTooltip.V1, but its trigger and overlay semantics are for dialog-like disclosures rather than hover hints."
        ]
    , Heading.h2 [ Heading.plaintext "Clipping demo" ]
    , clippedStage
        [ InfoPopover.view
            { id = "info-popover-demo"
            , triggerId = "info-popover-trigger"
            , label = "Planet details"
            , trigger = \attrs -> Html.button (Attributes.type_ "button" :: attrs) [ Html.text "Open details" ]
            }
            [ InfoPopover.html
                [ Html.p [ css [ Css.marginTop Css.zero ] ] [ Html.text "Mercury is the closest planet to the sun." ]
                , Html.a [ Attributes.href "#popover-link" ] [ Html.text "Read more" ]
                ]
            , InfoPopover.fitToContent
            , InfoPopover.onToggle ToggleClippingDemo
            , InfoPopover.open state.clippingDemoOpen
            ]
        ]
    , scrollStage
        [ InfoPopover.view
            { id = "info-popover-scroll"
            , triggerId = "info-popover-scroll-trigger"
            , label = "Scrollable details"
            , trigger = \attrs -> Html.button (Attributes.id "info-popover-scroll-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Open scroll demo" ]
            }
            [ InfoPopover.paragraph "This popover escapes an overflow auto container too."
            , InfoPopover.fitToContent
            , InfoPopover.onToggle ToggleScrollDemo
            , InfoPopover.open state.scrollDemoOpen
            ]
        ]
    , Heading.h2 [ Heading.plaintext "Positioning examples" ]
    , Html.div
        [ css
            [ Css.displayFlex
            , Css.property "gap" "24px"
            , Css.flexWrap Css.wrap
            , Css.marginBottom (Css.px 72)
            ]
        ]
        [ InfoPopover.view
            { id = "info-popover-right"
            , triggerId = "info-popover-right-trigger"
            , label = "Sync info"
            , trigger = \attrs -> Html.button (Attributes.id "info-popover-right-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Right aligned" ]
            }
            [ InfoPopover.paragraph "Popover content can include links and multiple elements."
            , InfoPopover.onRight
            , InfoPopover.alignEnd
            , InfoPopover.onToggle ToggleRightAligned
            , InfoPopover.open state.rightAlignedOpen
            ]
        , InfoPopover.view
            { id = "info-popover-flip"
            , triggerId = "info-popover-flip-trigger"
            , label = "Flip details"
            , trigger = \attrs -> Html.button (Attributes.id "info-popover-flip-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Flip near viewport top" ]
            }
            [ InfoPopover.paragraph "When you scroll this trigger to the top of the viewport, the popover should flip below it."
            , InfoPopover.onToggle ToggleFlip
            , InfoPopover.open state.flipOpen
            ]
        , InfoPopover.view
            { id = "info-popover-edge"
            , triggerId = "info-popover-edge-trigger"
            , label = "Edge details"
            , trigger = \attrs -> Html.button (Attributes.id "info-popover-edge-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Shift near edge" ]
            }
            [ InfoPopover.paragraph "This popover prefers the right side, but should stay inside the viewport."
            , InfoPopover.onRight
            , InfoPopover.onToggle ToggleEdge
            , InfoPopover.open state.edgeOpen
            ]
        , InfoPopover.view
            { id = "info-popover-mobile"
            , triggerId = "info-popover-mobile-trigger"
            , label = "Mobile fallback"
            , trigger = \attrs -> Html.button (Attributes.id "info-popover-mobile-trigger" :: Attributes.type_ "button" :: attrs) [ Html.text "Mobile fallback" ]
            }
            [ InfoPopover.paragraph "This example prefers right, but flips to bottom on mobile."
            , InfoPopover.onRight
            , InfoPopover.onBottomForMobile
            , InfoPopover.onToggle ToggleMobileFallback
            , InfoPopover.open state.mobileFallbackOpen
            ]
        ]
    ]


clippedStage : List (Html msg) -> Html msg
clippedStage children =
    Html.div
        [ Attributes.id "info-popover-clipping-demo"
        , css
            [ Css.marginTop (Css.px 20)
            , Css.width (Css.px 320)
            , Css.height (Css.px 130)
            , Css.padding (Css.px 16)
            , Css.borderRadius (Css.px 16)
            , Css.backgroundColor (Css.rgb 252 240 223)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 236 197 128)
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
        [ Attributes.id "info-popover-scroll-demo"
        , css
            [ Css.marginTop (Css.px 20)
            , Css.width (Css.px 320)
            , Css.height (Css.px 130)
            , Css.padding (Css.px 16)
            , Css.borderRadius (Css.px 16)
            , Css.backgroundColor (Css.rgb 252 240 223)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 236 197 128)
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
            , Css.backgroundColor (Css.rgb 252 240 223)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 236 197 128)
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
