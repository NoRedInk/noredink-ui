module Nri.Ui.AnchoredOverlay.Internal.V1 exposing
    ( Alignment(..)
    , Attribute
    , Config
    , HintPurpose(..)
    , Mode(..)
    , Placement(..)
    , alignEnd
    , alignMiddle
    , alignStart
    , applyAttribute
    , auxiliaryDescription
    , buildAttributes
    , css
    , customPadding
    , exactWidth
    , fitToContent
    , helpfullyDisabled
    , html
    , markdown
    , normalPadding
    , nriDescription
    , onBottom
    , onBottomForMobile
    , onLeft
    , onLeftForMobile
    , onRight
    , onRightForMobile
    , onTop
    , onTopForMobile
    , onToggle
    , onTriggerKeyDown
    , open
    , paragraph
    , plaintext
    , primaryLabel
    , smallPadding
    , testId
    , view
    , withoutTail
    )

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Content
import Css exposing (Style)
import Css.Global as Global
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Shadows.V1 as Shadows


type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { placement : Placement
    , mobilePlacement : Maybe Placement
    , alignment : Alignment
    , hasTail : Bool
    , content : List (Html msg)
    , bubbleAttributes : List (Root.Attribute Never)
    , bubbleStyles : List Style
    , width : Width
    , padding : Padding
    , triggerKeyDownEvents : List (Key.Event msg)
    , isOpen : Bool
    , onToggle : Maybe (Bool -> msg)
    , hintPurpose : HintPurpose
    }


type Mode
    = Hint
    | Popover


type Placement
    = Top
    | Bottom
    | Left
    | Right


type Alignment
    = Start
    | Middle
    | End


type HintPurpose
    = PrimaryLabel
    | AuxiliaryDescription
    | HelpfullyDisabled


type Width
    = Exactly Int
    | FitToContent


type Padding
    = SmallPadding
    | NormalPadding
    | CustomPadding Float


defaultConfig : Config msg
defaultConfig =
    { placement = Top
    , mobilePlacement = Nothing
    , alignment = Middle
    , hasTail = True
    , content = []
    , bubbleAttributes = []
    , bubbleStyles = []
    , width = Exactly 320
    , padding = NormalPadding
    , triggerKeyDownEvents = []
    , isOpen = False
    , onToggle = Nothing
    , hintPurpose = PrimaryLabel
    }


applyAttribute : Attribute msg -> Config msg -> Config msg
applyAttribute (Attribute update) config =
    update config


buildAttributes : List (Attribute msg) -> Config msg
buildAttributes =
    List.foldl applyAttribute defaultConfig


plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


paragraph : String -> Attribute msg
paragraph =
    Attribute << Content.paragraph


markdown : String -> Attribute msg
markdown string =
    Attribute (\config -> { config | content = Content.markdownInline string })


html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html


withoutTail : Attribute msg
withoutTail =
    Attribute (\config -> { config | hasTail = False })


onTop : Attribute msg
onTop =
    Attribute (\config -> { config | placement = Top })


onBottom : Attribute msg
onBottom =
    Attribute (\config -> { config | placement = Bottom })


onLeft : Attribute msg
onLeft =
    Attribute (\config -> { config | placement = Left })


onRight : Attribute msg
onRight =
    Attribute (\config -> { config | placement = Right })


onTopForMobile : Attribute msg
onTopForMobile =
    Attribute (\config -> { config | mobilePlacement = Just Top })


onBottomForMobile : Attribute msg
onBottomForMobile =
    Attribute (\config -> { config | mobilePlacement = Just Bottom })


onLeftForMobile : Attribute msg
onLeftForMobile =
    Attribute (\config -> { config | mobilePlacement = Just Left })


onRightForMobile : Attribute msg
onRightForMobile =
    Attribute (\config -> { config | mobilePlacement = Just Right })


alignStart : Attribute msg
alignStart =
    Attribute (\config -> { config | alignment = Start })


alignMiddle : Attribute msg
alignMiddle =
    Attribute (\config -> { config | alignment = Middle })


alignEnd : Attribute msg
alignEnd =
    Attribute (\config -> { config | alignment = End })


exactWidth : Int -> Attribute msg
exactWidth width =
    Attribute (\config -> { config | width = Exactly width })


fitToContent : Attribute msg
fitToContent =
    Attribute (\config -> { config | width = FitToContent })


smallPadding : Attribute msg
smallPadding =
    Attribute (\config -> { config | padding = SmallPadding })


normalPadding : Attribute msg
normalPadding =
    Attribute (\config -> { config | padding = NormalPadding })


customPadding : Float -> Attribute msg
customPadding value =
    Attribute (\config -> { config | padding = CustomPadding value })


open : Bool -> Attribute msg
open isOpen =
    Attribute (\config -> { config | isOpen = isOpen })


onToggle : (Bool -> msg) -> Attribute msg
onToggle toMsg =
    Attribute (\config -> { config | onToggle = Just toMsg })


onTriggerKeyDown : List (Key.Event msg) -> Attribute msg
onTriggerKeyDown events =
    Attribute (\config -> { config | triggerKeyDownEvents = events })


css : List Style -> Attribute msg
css styles =
    Attribute (\config -> { config | bubbleStyles = config.bubbleStyles ++ styles })


testId : String -> Attribute msg
testId id_ =
    Attribute (\config -> { config | bubbleAttributes = config.bubbleAttributes ++ [ ExtraAttributes.testId id_ ] })


nriDescription : String -> Attribute msg
nriDescription description =
    Attribute (\config -> { config | bubbleAttributes = config.bubbleAttributes ++ [ ExtraAttributes.nriDescription description ] })


primaryLabel : Attribute msg
primaryLabel =
    Attribute (\config -> { config | hintPurpose = PrimaryLabel })


auxiliaryDescription : Attribute msg
auxiliaryDescription =
    Attribute (\config -> { config | hintPurpose = AuxiliaryDescription })


helpfullyDisabled : Attribute msg
helpfullyDisabled =
    Attribute (\config -> { config | hintPurpose = HelpfullyDisabled })


view :
    { id : String
    , anchorId : String
    , trigger : List (Html.Attribute msg) -> Html msg
    , mode : Mode
    , wrapperDescription : String
    , wrapperEvents : List (Html.Attribute msg)
    , triggerAttributes : Config msg -> List (Html.Attribute msg)
    , hostAttributes : Config msg -> List (Html.Attribute msg)
    }
    -> Config msg
    -> Html msg
view config overlay =
    let
        hostClassName =
            "Nri-Ui-AnchoredOverlay-V1-host"

        bubbleClassName =
            "Nri-Ui-AnchoredOverlay-V1-bubble"
    in
    Root.div
        ([ Attributes.id config.anchorId
         , Attributes.css
            [ Css.display Css.inlineBlock
            , Css.position Css.relative
            , Css.textAlign Css.left
            ]
         , ExtraAttributes.nriDescription config.wrapperDescription
         ]
            ++ config.wrapperEvents
        )
        [ tailStyles hostClassName bubbleClassName
        , config.trigger (config.triggerAttributes overlay)
        , Root.node "nri-anchored-overlay-v1"
            ([ Attributes.id config.id
             , Attributes.class hostClassName
             , Attributes.attribute "data-mode" (modeToString config.mode)
             , Attributes.attribute "data-trigger-id" config.anchorId
             , Attributes.attribute "data-placement" (placementToString overlay.placement)
             , Attributes.attribute "data-alignment" (alignmentToString overlay.alignment)
             , Attributes.attribute "data-gap" (String.fromInt gapPx)
             , Attributes.attribute "data-viewport-padding" (String.fromInt viewportPaddingPx)
             , Attributes.attribute "data-resolved-placement" (placementToString overlay.placement)
             , Attributes.attribute "data-resolved-alignment" (alignmentToString overlay.alignment)
             , Attributes.attribute "data-overlay-visible"
                (if overlay.isOpen then
                    "true"

                 else
                    "false"
                )
             , ExtraAttributes.maybe (placementToString >> Attributes.attribute "data-mobile-placement") overlay.mobilePlacement
             , Attributes.css (hostStyles hostClassName bubbleClassName overlay)
             , if overlay.isOpen then
                Attributes.attribute "open" ""

               else
                ExtraAttributes.none
             ]
                ++ config.hostAttributes overlay
            )
            [ Root.div
                ([ Attributes.class bubbleClassName
                 , Attributes.attribute "data-nri-anchored-overlay-bubble" ""
                 , Attributes.attribute "data-has-tail"
                    (if overlay.hasTail then
                        "true"

                     else
                        "false"
                    )
                 , Attributes.css (bubbleStyles config.mode overlay)
                 ]
                    ++ List.map (Attributes.map never) overlay.bubbleAttributes
                )
                (overlay.content ++ hoverBridge config.mode)
            ]
        ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        Hint ->
            "hint"

        Popover ->
            "popover"


placementToString : Placement -> String
placementToString placement =
    case placement of
        Top ->
            "top"

        Bottom ->
            "bottom"

        Left ->
            "left"

        Right ->
            "right"


alignmentToString : Alignment -> String
alignmentToString alignment =
    case alignment of
        Start ->
            "start"

        Middle ->
            "middle"

        End ->
            "end"


bubbleStyles : Mode -> Config msg -> List Style
bubbleStyles mode overlay =
    [ Css.boxSizing Css.borderBox
    , Css.borderRadius (Css.px 8)
    , Css.position Css.relative
    , Css.backgroundColor Colors.navy
    , Css.border3 (Css.px 1) Css.solid outlineColor
    , paddingToStyle overlay.padding
    , Fonts.baseFont
    , Css.fontSize (Css.px 15)
    , Css.fontWeight (Css.int 600)
    , Css.color Colors.white
    , Shadows.high
    , Css.pointerEvents Css.auto
    , Css.property "max-width" "calc(100vw - 24px)"
    , case overlay.width of
        Exactly width ->
            Css.width (Css.px (toFloat width))

        FitToContent ->
            case mode of
                Hint ->
                    Css.batch
                        [ Css.whiteSpace Css.noWrap
                        , Css.property "width" "max-content"
                        ]

                Popover ->
                    Css.property "width" "max-content"
    , Global.descendants
        [ Global.a
            [ Css.color Colors.white
            , Css.borderColor Colors.white
            , Css.textDecoration Css.none
            , Css.borderBottom3 (Css.px 1) Css.solid Colors.white
            , Css.visited [ Css.color Colors.white ]
            , Css.hover [ Css.color Colors.white ]
            ]
        ]
    ]
        ++ overlay.bubbleStyles


paddingToStyle : Padding -> Style
paddingToStyle padding =
    case padding of
        SmallPadding ->
            Css.padding2 (Css.px 10) (Css.px 13)

        NormalPadding ->
            Css.padding (Css.px 20)

        CustomPadding value ->
            Css.padding (Css.px value)


hostStyles : String -> String -> Config msg -> List Style
hostStyles hostClassName bubbleClassName overlay =
    [ Css.position Css.fixed
    , Css.top Css.zero
    , Css.left Css.zero
    , Css.zIndex (Css.int 1000)
    , Css.margin Css.zero
    , Css.padding Css.zero
    , Css.border Css.zero
    , Css.backgroundColor Css.transparent
    , Css.overflow Css.visible
    , if overlay.isOpen then
        Css.display Css.block

      else
        Css.display Css.none
    ]


tailStyles : String -> String -> Html msg
tailStyles hostClassName bubbleClassName =
    Global.global
        [ Global.selector ("." ++ hostClassName ++ "[data-resolved-placement='top'] > ." ++ bubbleClassName ++ "[data-has-tail='true']") [ bottomTail ]
        , Global.selector ("." ++ hostClassName ++ "[data-resolved-placement='bottom'] > ." ++ bubbleClassName ++ "[data-has-tail='true']") [ topTail ]
        , Global.selector ("." ++ hostClassName ++ "[data-resolved-placement='left'] > ." ++ bubbleClassName ++ "[data-has-tail='true']") [ rightTail ]
        , Global.selector ("." ++ hostClassName ++ "[data-resolved-placement='right'] > ." ++ bubbleClassName ++ "[data-has-tail='true']") [ leftTail ]
        , Global.selector ("." ++ hostClassName ++ "[data-mode='hint'][data-resolved-placement='top'] > ." ++ bubbleClassName ++ " > [data-nri-anchored-overlay-hover-bridge]") topHoverBridge
        , Global.selector ("." ++ hostClassName ++ "[data-mode='hint'][data-resolved-placement='bottom'] > ." ++ bubbleClassName ++ " > [data-nri-anchored-overlay-hover-bridge]") bottomHoverBridge
        , Global.selector ("." ++ hostClassName ++ "[data-mode='hint'][data-resolved-placement='left'] > ." ++ bubbleClassName ++ " > [data-nri-anchored-overlay-hover-bridge]") leftHoverBridge
        , Global.selector ("." ++ hostClassName ++ "[data-mode='hint'][data-resolved-placement='right'] > ." ++ bubbleClassName ++ " > [data-nri-anchored-overlay-hover-bridge]") rightHoverBridge
        ]


hoverBridge : Mode -> List (Html msg)
hoverBridge mode =
    case mode of
        Hint ->
            [ Root.div
                [ ExtraAttributes.nriDescription "anchored-overlay-hover-bridge"
                , Attributes.attribute "data-nri-anchored-overlay-hover-bridge" ""
                , Attributes.css
                    [ Css.position Css.absolute
                    , Css.backgroundColor Css.transparent
                    ]
                ]
                []
            ]

        Popover ->
            []


tailSize : Float
tailSize =
    8


hoverBridgeSize : Float
hoverBridgeSize =
    23


gapPx : Int
gapPx =
    12


viewportPaddingPx : Int
viewportPaddingPx =
    12


outlineColor : Css.Color
outlineColor =
    Css.rgba 255 255 255 0.5


bottomTail : Style
bottomTail =
    Css.batch
        [ Css.before
            (horizontalTail
                [ Css.top (Css.pct 100)
                , Css.borderTopColor outlineColor
                , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
                , Css.marginLeft (Css.px (-tailSize - 1))
                ]
            )
        , Css.after
            (horizontalTail
                [ Css.top (Css.pct 100)
                , Css.borderTopColor Colors.navy
                , Css.property "border-width" (String.fromFloat tailSize ++ "px")
                , Css.marginLeft (Css.px -tailSize)
                ]
            )
        ]


topTail : Style
topTail =
    Css.batch
        [ Css.before
            (horizontalTail
                [ Css.bottom (Css.pct 100)
                , Css.borderBottomColor outlineColor
                , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
                , Css.marginLeft (Css.px (-tailSize - 1))
                ]
            )
        , Css.after
            (horizontalTail
                [ Css.bottom (Css.pct 100)
                , Css.borderBottomColor Colors.navy
                , Css.property "border-width" (String.fromFloat tailSize ++ "px")
                , Css.marginLeft (Css.px -tailSize)
                ]
            )
        ]


rightTail : Style
rightTail =
    Css.batch
        [ Css.before
            (verticalTail
                [ Css.left (Css.pct 100)
                , Css.borderLeftColor outlineColor
                , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
                , Css.marginTop (Css.px (-tailSize - 1))
                ]
            )
        , Css.after
            (verticalTail
                [ Css.left (Css.pct 100)
                , Css.borderLeftColor Colors.navy
                , Css.property "border-width" (String.fromFloat tailSize ++ "px")
                , Css.marginTop (Css.px -tailSize)
                ]
            )
        ]


leftTail : Style
leftTail =
    Css.batch
        [ Css.before
            (verticalTail
                [ Css.right (Css.pct 100)
                , Css.borderRightColor outlineColor
                , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
                , Css.marginTop (Css.px (-tailSize - 1))
                ]
            )
        , Css.after
            (verticalTail
                [ Css.right (Css.pct 100)
                , Css.borderRightColor Colors.navy
                , Css.property "border-width" (String.fromFloat tailSize ++ "px")
                , Css.marginTop (Css.px -tailSize)
                ]
            )
        ]


horizontalTail : List Style -> List Style
horizontalTail styles =
    [ Css.property "content" "\"\""
    , Css.position Css.absolute
    , Css.property "left" "var(--nri-overlay-tail-offset, 50%)"
    , Css.width Css.zero
    , Css.height Css.zero
    , Css.property "border-style" "solid"
    , Css.property "border-color" "transparent"
    , Css.pointerEvents Css.none
    ]
        ++ styles


verticalTail : List Style -> List Style
verticalTail styles =
    [ Css.property "content" "\"\""
    , Css.position Css.absolute
    , Css.property "top" "var(--nri-overlay-tail-offset, 50%)"
    , Css.width Css.zero
    , Css.height Css.zero
    , Css.property "border-style" "solid"
    , Css.property "border-color" "transparent"
    , Css.pointerEvents Css.none
    ]
        ++ styles


topHoverBridge : List Style
topHoverBridge =
    [ Css.bottom (Css.pct 100)
    , Css.left Css.zero
    , Css.right Css.zero
    , Css.height (Css.px hoverBridgeSize)
    ]


bottomHoverBridge : List Style
bottomHoverBridge =
    [ Css.top (Css.pct 100)
    , Css.left Css.zero
    , Css.right Css.zero
    , Css.height (Css.px hoverBridgeSize)
    ]


leftHoverBridge : List Style
leftHoverBridge =
    [ Css.right (Css.pct 100)
    , Css.top Css.zero
    , Css.bottom Css.zero
    , Css.width (Css.px hoverBridgeSize)
    ]


rightHoverBridge : List Style
rightHoverBridge =
    [ Css.left (Css.pct 100)
    , Css.top Css.zero
    , Css.bottom Css.zero
    , Css.width (Css.px hoverBridgeSize)
    ]
