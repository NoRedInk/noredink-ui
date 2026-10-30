module Nri.Ui.Tooltip.V4 exposing
    ( view
    , Attribute
    , plaintext, paragraph, markdown, html
    , above, below, before, after
    , Align, alignStart, alignMiddle, alignEnd, align
    , flip, noFlip
    , offset
    , withoutTail
    , forBreakpoint, Breakpoint, mobile, narrowMobile, quizEngineMobile
    , width, fitToContent, exactWidth
    , Padding, smallPadding, normalPadding, customPadding, padding
    , onToggle, onTriggerKeyDown
    , open
    , primaryLabel, auxiliaryDescription, helpfullyDisabled, disclosure
    , css, custom, nriDescription, testId
    )

{-| A consolidated, auto-flipping rebuild of `Nri.Ui.Tooltip.V3`.

This is a clean break: the API has been simplified considerably. See the
migration table in the PR description.

Auto-flipping is **on by default**. The direction passed via `above`,
`below`, `before`, or `after` is the *preferred* placement. If the tooltip
would clip the viewport on that side, it moves to the opposite side.
Tail alignment shifts the same way to keep the tail pointing at the
trigger.

Powers itself with the `<nri-tooltip-auto>` custom element registered in
`lib/Tooltip/V4.js`. Make sure your bundle requires `noredink-ui/lib`
or the equivalent.

@docs view
@docs Attribute


## Content

@docs plaintext, paragraph, markdown, html


## Position

@docs above, below, before, after
@docs Align, alignStart, alignMiddle, alignEnd, align
@docs flip, noFlip
@docs offset
@docs withoutTail


## Responsive

@docs forBreakpoint, Breakpoint, mobile, narrowMobile, quizEngineMobile


## Size & padding

@docs width, fitToContent, exactWidth
@docs Padding, smallPadding, normalPadding, customPadding, padding


## Behaviour

@docs onToggle, onTriggerKeyDown
@docs open


## Purpose (a11y)

@docs primaryLabel, auxiliaryDescription, helpfullyDisabled, disclosure


## Escape hatches

@docs css, custom, nriDescription, testId

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Content
import Css exposing (Color, Px, Style)
import Css.Global
import Css.Media
import EventExtras
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.WhenFocusLeaves.V2 as WhenFocusLeaves



-- POSITION TYPES


{-| Internal: where the tooltip sits relative to the trigger.
-}
type Position
    = Top
    | Bottom
    | Left
    | Right


positionToString : Position -> String
positionToString p =
    case p of
        Top ->
            "top"

        Bottom ->
            "bottom"

        Left ->
            "left"

        Right ->
            "right"


{-| -}
type Align
    = AlignStart
    | AlignMiddle
    | AlignEnd


alignToString : Align -> String
alignToString a =
    case a of
        AlignStart ->
            "start"

        AlignMiddle ->
            "middle"

        AlignEnd ->
            "end"



-- BREAKPOINTS


{-| Named viewport size at which a responsive override applies.
-}
type Breakpoint
    = Mobile
    | QuizEngineMobile
    | NarrowMobile


{-| -}
mobile : Breakpoint
mobile =
    Mobile


{-| -}
quizEngineMobile : Breakpoint
quizEngineMobile =
    QuizEngineMobile


{-| -}
narrowMobile : Breakpoint
narrowMobile =
    NarrowMobile


breakpointMaxPx : Breakpoint -> Css.Px
breakpointMaxPx bp =
    case bp of
        Mobile ->
            MediaQuery.mobileBreakpoint

        QuizEngineMobile ->
            MediaQuery.quizEngineBreakpoint

        NarrowMobile ->
            MediaQuery.narrowMobileBreakpoint



-- ATTRIBUTES


{-| -}
type Attribute msg
    = Attribute (Tooltip msg -> Tooltip msg)


type alias Tooltip msg =
    { position : Position
    , align : Align
    , flipEnabled : Bool
    , offsetPx : Float
    , withTail : Bool
    , content : List (Html msg)
    , extraStyles : List Style
    , extraAttributes : List (Root.Attribute Never)
    , width : Width
    , padding : Padding
    , trigger : Maybe (Trigger msg)
    , triggerKeyDownEvents : List (Key.Event msg)
    , purpose : Purpose
    , isOpen : Bool
    , breakpointOverrides : List ( Breakpoint, Override )
    , breakpointStyles : List ( Breakpoint, List Style )
    }


type Width
    = Exactly Int
    | FitContent


type Padding
    = SmallPadding
    | NormalPadding
    | CustomPadding Float


type Trigger msg
    = OnHover (Bool -> msg)


type Purpose
    = PrimaryLabel
    | AuxiliaryDescription
    | HelpfullyDisabled
    | Disclosure { triggerId : String, lastId : Maybe String }


type alias Override =
    { position : Maybe Position
    , align : Maybe Align
    }


emptyOverride : Override
emptyOverride =
    { position = Nothing, align = Nothing }


buildAttributes : List (Attribute msg) -> Tooltip msg
buildAttributes =
    let
        defaults : Tooltip msg
        defaults =
            { position = Top
            , align = AlignMiddle
            , flipEnabled = True
            , offsetPx = 12
            , withTail = True
            , content = []
            , extraStyles = []
            , extraAttributes = []
            , width = Exactly 320
            , padding = NormalPadding
            , trigger = Nothing
            , triggerKeyDownEvents = []
            , purpose = PrimaryLabel
            , isOpen = False
            , breakpointOverrides = []
            , breakpointStyles = []
            }
    in
    List.foldl (\(Attribute f) acc -> f acc) defaults



-- CONTENT


{-| -}
plaintext : String -> Attribute msg
plaintext str =
    Attribute (Content.plaintext str)


{-| -}
paragraph : String -> Attribute msg
paragraph str =
    Attribute (Content.paragraph str)


{-| -}
markdown : String -> Attribute msg
markdown str =
    Attribute (\t -> { t | content = Content.markdownInline str })


{-| -}
html : List (Html msg) -> Attribute msg
html nodes =
    Attribute (Content.html nodes)



-- POSITION


{-| Prefer to place the tooltip above the trigger.
-}
above : Attribute msg
above =
    Attribute (\t -> { t | position = Top })


{-| Prefer to place the tooltip below the trigger.
-}
below : Attribute msg
below =
    Attribute (\t -> { t | position = Bottom })


{-| Prefer to place the tooltip before the trigger (to its left in LTR).
-}
before : Attribute msg
before =
    Attribute (\t -> { t | position = Left })


{-| Prefer to place the tooltip after the trigger (to its right in LTR).
-}
after : Attribute msg
after =
    Attribute (\t -> { t | position = Right })


{-| -}
align : Align -> Attribute msg
align a =
    Attribute (\t -> { t | align = a })


{-| -}
alignStart : Attribute msg
alignStart =
    align AlignStart


{-| -}
alignMiddle : Attribute msg
alignMiddle =
    align AlignMiddle


{-| -}
alignEnd : Attribute msg
alignEnd =
    align AlignEnd


{-| Auto-flipping is on by default; this is here for symmetry.
-}
flip : Attribute msg
flip =
    Attribute (\t -> { t | flipEnabled = True })


{-| Disable auto-flipping. The tooltip will stay on the preferred side
even if it clips the viewport.
-}
noFlip : Attribute msg
noFlip =
    Attribute (\t -> { t | flipEnabled = False })


{-| Pixel gap between the trigger and the tooltip. Default is 8px.
-}
offset : Float -> Attribute msg
offset px =
    Attribute (\t -> { t | offsetPx = px })


{-| -}
withoutTail : Attribute msg
withoutTail =
    Attribute (\t -> { t | withTail = False })



-- RESPONSIVE


{-| Apply position/align overrides at a specific breakpoint.

    Tooltip.forBreakpoint Tooltip.mobile [ Tooltip.below, Tooltip.alignStart ]

The overrides are merged onto the base configuration in order; smaller
breakpoints inherit from larger ones.

-}
forBreakpoint : Breakpoint -> List (Attribute msg) -> Attribute msg
forBreakpoint bp atts =
    Attribute
        (\t ->
            let
                folded =
                    List.foldl (\(Attribute f) acc -> f acc)
                        { t | extraStyles = [], breakpointStyles = [] }
                        atts

                override =
                    { position =
                        if folded.position /= t.position then
                            Just folded.position

                        else
                            Nothing
                    , align =
                        if folded.align /= t.align then
                            Just folded.align

                        else
                            Nothing
                    }
            in
            { t
                | breakpointOverrides = ( bp, override ) :: t.breakpointOverrides
                , breakpointStyles =
                    if List.isEmpty folded.extraStyles then
                        t.breakpointStyles

                    else
                        ( bp, folded.extraStyles ) :: t.breakpointStyles
            }
        )



-- SIZE / PADDING


{-| -}
width : Int -> Attribute msg
width w =
    Attribute (\t -> { t | width = Exactly w })


{-| -}
exactWidth : Int -> Attribute msg
exactWidth =
    width


{-| -}
fitToContent : Attribute msg
fitToContent =
    Attribute (\t -> { t | width = FitContent })


{-| -}
padding : Padding -> Attribute msg
padding p =
    Attribute (\t -> { t | padding = p })


{-| -}
smallPadding : Attribute msg
smallPadding =
    padding SmallPadding


{-| -}
normalPadding : Attribute msg
normalPadding =
    padding NormalPadding


{-| -}
customPadding : Float -> Attribute msg
customPadding px =
    padding (CustomPadding px)



-- BEHAVIOUR


{-| -}
onToggle : (Bool -> msg) -> Attribute msg
onToggle msg =
    Attribute (\t -> { t | trigger = Just (OnHover msg) })


{-| -}
onTriggerKeyDown : List (Key.Event msg) -> Attribute msg
onTriggerKeyDown events =
    Attribute (\t -> { t | triggerKeyDownEvents = t.triggerKeyDownEvents ++ events })


{-| -}
open : Bool -> Attribute msg
open isOpen =
    Attribute (\t -> { t | isOpen = isOpen })



-- PURPOSE


{-| -}
primaryLabel : Attribute msg
primaryLabel =
    Attribute (\t -> { t | purpose = PrimaryLabel })


{-| -}
auxiliaryDescription : Attribute msg
auxiliaryDescription =
    Attribute (\t -> { t | purpose = AuxiliaryDescription })


{-| -}
helpfullyDisabled : Attribute msg
helpfullyDisabled =
    Attribute (\t -> { t | purpose = HelpfullyDisabled })


{-| -}
disclosure : { triggerId : String, lastId : Maybe String } -> Attribute msg
disclosure config =
    Attribute (\t -> { t | purpose = Disclosure config })



-- ESCAPE HATCHES


{-| -}
css : List Style -> Attribute msg
css styles =
    Attribute (\t -> { t | extraStyles = t.extraStyles ++ styles })


{-| -}
custom : List (Root.Attribute Never) -> Attribute msg
custom atts =
    Attribute (\t -> { t | extraAttributes = t.extraAttributes ++ atts })


{-| -}
nriDescription : String -> Attribute msg
nriDescription desc =
    custom [ ExtraAttributes.nriDescription desc ]


{-| -}
testId : String -> Attribute msg
testId id =
    custom [ ExtraAttributes.testId id ]



-- VIEW


{-| -}
view :
    { trigger : List (Root.Attribute msg) -> Html msg
    , id : String
    }
    -> List (Attribute msg)
    -> Html msg
view config attrs =
    let
        tooltip =
            buildAttributes attrs

        triggerId =
            "tooltip-trigger__" ++ config.id

        ( containerEvents, triggerEvents ) =
            triggerEventHandlers tooltip
    in
    Nri.Ui.styled Root.div
        "Nri-Ui-Tooltip-V4"
        [ Css.boxSizing Css.borderBox
        , Css.display Css.inlineBlock
        , Css.textAlign Css.left
        , Css.position Css.relative
        ]
        containerEvents
        [ Root.div
            [ Attributes.css [ Css.displayFlex ]
            , Attributes.id triggerId
            ]
            [ config.trigger
                (purposeAttributes config.id tooltip ++ triggerEvents)
            ]
        , wrapInAuto tooltip
            { triggerId = triggerId, tooltipId = config.id }
            (viewTooltip config.id triggerId tooltip)
        ]


wrapInAuto :
    Tooltip msg
    -> { triggerId : String, tooltipId : String }
    -> Html msg
    -> Html msg
wrapInAuto tooltip ids inner =
    if tooltip.flipEnabled then
        Root.node "nri-tooltip-auto"
            [ Attributes.attribute "data-trigger-id" ids.triggerId
            , Attributes.attribute "data-tooltip-id" ids.tooltipId
            , Attributes.attribute "data-preferred-position"
                (positionToString tooltip.position)
            , Attributes.attribute "data-preferred-align"
                (alignToString tooltip.align)
            , Attributes.attribute "data-offset"
                (String.fromFloat tooltip.offsetPx)
            , Attributes.css [ Css.property "display" "contents" ]
            ]
            [ inner ]

    else
        inner


purposeAttributes : String -> Tooltip msg -> List (Root.Attribute msg)
purposeAttributes id tooltip =
    case tooltip.purpose of
        PrimaryLabel ->
            []

        AuxiliaryDescription ->
            [ Aria.describedBy [ id ] ]

        HelpfullyDisabled ->
            [ Aria.describedBy [ id ] ]

        Disclosure _ ->
            [ Aria.expanded tooltip.isOpen
            , Aria.controls [ id ]
            ]


triggerEventHandlers :
    Tooltip msg
    -> ( List (Root.Attribute msg), List (Root.Attribute msg) )
triggerEventHandlers tooltip =
    case tooltip.trigger of
        Just (OnHover msg) ->
            case tooltip.purpose of
                Disclosure { triggerId, lastId } ->
                    ( [ Events.onMouseEnter (msg True)
                      , Events.onMouseLeave (msg False)
                      , WhenFocusLeaves.onKeyDown []
                            { firstIds = [ triggerId ]
                            , lastIds = [ Maybe.withDefault triggerId lastId ]
                            , tabBackAction = msg False
                            , tabForwardAction = msg False
                            }
                      ]
                    , [ EventExtras.onClickPreventDefaultAndStopPropagation
                            (msg (not tooltip.isOpen))
                      , Key.onKeyDown
                            (Key.escape (msg False) :: tooltip.triggerKeyDownEvents)
                      ]
                    )

                _ ->
                    ( [ Events.onMouseEnter (msg True)
                      , Events.onMouseLeave (msg False)
                      ]
                    , [ Events.onFocus (msg True)
                      , Events.onBlur (msg False)
                      , Key.onKeyDown
                            (Key.escape (msg False) :: tooltip.triggerKeyDownEvents)
                      ]
                    )

        Nothing ->
            ( [], [ Key.onKeyDown tooltip.triggerKeyDownEvents ] )


viewTooltip : String -> String -> Tooltip msg -> Html msg
viewTooltip tooltipId triggerId config =
    Root.div
        ([ Attributes.id tooltipId
         , Attributes.attribute "data-nri-tooltip" "v4"
         , Attributes.attribute "data-position" (positionToString config.position)
         , Attributes.attribute "data-align" (alignToString config.align)
         , Attributes.attribute "data-tail"
            (if config.withTail then
                "shown"

             else
                "hidden"
            )
         , Attributes.attribute "data-tooltip-visible"
            (if config.isOpen then
                "true"

             else
                "false"
            )
         , Aria.hidden (config.purpose == PrimaryLabel)
         , Attributes.css
            (containerStyles config
                ++ List.concatMap breakpointBlock config.breakpointOverrides
                ++ List.concatMap breakpointStyleBlock config.breakpointStyles
            )
         ]
            ++ List.map (Attributes.map never) config.extraAttributes
        )
        [ Css.Global.global tooltipScopedStyles
        , Root.div
            [ Attributes.class bubbleClass
            , Attributes.css (innerStyles config)
            ]
            config.content
        ]


breakpointBlock : ( Breakpoint, Override ) -> List Style
breakpointBlock ( bp, override ) =
    let
        styles =
            List.filterMap identity
                [ Maybe.map overridePositionStyle override.position
                , Maybe.map overrideAlignStyle override.align
                ]
    in
    if List.isEmpty styles then
        []

    else
        [ Css.Media.withMedia
            [ Css.Media.only Css.Media.screen
                [ Css.Media.maxWidth (breakpointMaxPx bp) ]
            ]
            styles
        ]


breakpointStyleBlock : ( Breakpoint, List Style ) -> List Style
breakpointStyleBlock ( bp, styles ) =
    [ Css.Media.withMedia
        [ Css.Media.only Css.Media.screen
            [ Css.Media.maxWidth (breakpointMaxPx bp) ]
        ]
        styles
    ]


overridePositionStyle : Position -> Style
overridePositionStyle p =
    Css.batch (positionStyles p)


overrideAlignStyle : Align -> Style
overrideAlignStyle a =
    Css.batch (alignStyles a)



-- STYLES


containerStyles : Tooltip msg -> List Style
containerStyles config =
    [ Css.position Css.absolute
    , Css.boxSizing Css.borderBox
    , Css.zIndex (Css.int 100)
    , if config.isOpen then
        Css.batch []

      else
        Css.display Css.none
    , Css.batch (positionStyles config.position)
    , Css.batch (alignStyles config.align)
    , Css.property "--nri-tooltip-offset"
        (String.fromFloat config.offsetPx ++ "px")
    ]


positionStyles : Position -> List Style
positionStyles p =
    case p of
        Top ->
            [ Css.property "bottom" "calc(100% + var(--nri-tooltip-offset, 12px))" ]

        Bottom ->
            [ Css.property "top" "calc(100% + var(--nri-tooltip-offset, 12px))" ]

        Left ->
            [ Css.property "right" "calc(100% + var(--nri-tooltip-offset, 12px))" ]

        Right ->
            [ Css.property "left" "calc(100% + var(--nri-tooltip-offset, 12px))" ]


alignStyles : Align -> List Style
alignStyles a =
    case a of
        AlignStart ->
            [ Css.property "left" "0"
            , Css.property "right" "auto"
            , Css.property "transform" "none"
            ]

        AlignMiddle ->
            [ Css.property "left" "50%"
            , Css.property "right" "auto"
            , Css.property "transform" "translateX(-50%)"
            ]

        AlignEnd ->
            [ Css.property "right" "0"
            , Css.property "left" "auto"
            , Css.property "transform" "none"
            ]


innerStyles : Tooltip msg -> List Style
innerStyles config =
    [ Css.boxSizing Css.borderBox
    , Css.position Css.relative
    , Css.borderRadius (Css.px 8)
    , case config.width of
        Exactly w ->
            Css.width (Css.px (toFloat w))

        FitContent ->
            Css.whiteSpace Css.noWrap
    , paddingToStyle config.padding
    , Css.backgroundColor tooltipColor
    , Css.color Colors.white
    , Css.border3 (Css.px 1) Css.solid outlineColor
    , Shadows.high
    , Fonts.baseFont
    , Css.fontSize (Css.px 15)
    , Css.lineHeight (Css.num 1.4)
    , Css.batch config.extraStyles
    ]


paddingToStyle : Padding -> Style
paddingToStyle p =
    case p of
        SmallPadding ->
            Css.padding2 (Css.px 8) (Css.px 12)

        NormalPadding ->
            Css.padding (Css.px 16)

        CustomPadding px ->
            Css.padding (Css.px px)



-- ATTRIBUTE-KEYED OVERRIDE STYLES (used by the custom element)
--
-- The custom element rewrites `data-position` and `data-align` on the
-- outer tooltip div. These global styles re-apply the corresponding
-- positioning so the flip happens without a re-render from Elm.


tooltipScopedStyles : List Css.Global.Snippet
tooltipScopedStyles =
    let
        scope =
            "[data-nri-tooltip=\"v4\"]"

        positionRule pos =
            Css.Global.selector
                (scope ++ "[data-position=\"" ++ positionToString pos ++ "\"]")
                (positionStyles pos)

        alignRule a =
            Css.Global.selector
                (scope ++ "[data-align=\"" ++ alignToString a ++ "\"]")
                (alignStyles a)
    in
    [ positionRule Top
    , positionRule Bottom
    , positionRule Left
    , positionRule Right
    , alignRule AlignStart
    , alignRule AlignMiddle
    , alignRule AlignEnd
    ]
        ++ tailScopedStyles


bubbleClass : String
bubbleClass =
    "nri-tooltip-v4-bubble"


tailSize : Float
tailSize =
    8


{-| Tail pseudo-element styles, scoped by `[data-position]` and
`[data-align]` so the JS auto-flip can move the tail with the tooltip
without an Elm re-render.

Each tail uses two pseudo-elements:

  - `::before` is the outer outline (border alpha)
  - `::after` is the inner fill (tooltip background)

Both are pure CSS triangles via the classic border-trick.

-}
tailScopedStyles : List Css.Global.Snippet
tailScopedStyles =
    let
        scope =
            "[data-nri-tooltip=\"v4\"]"

        bubble pos =
            scope ++ "[data-position=\"" ++ positionToString pos ++ "\"] > ." ++ bubbleClass

        tailBase =
            [ Css.property "content" "\" \""
            , Css.position Css.absolute
            , Css.height Css.zero
            , Css.width Css.zero
            , Css.pointerEvents Css.none
            , Css.property "border" "solid transparent"
            ]

        tailHidden =
            Css.Global.selector
                (scope ++ "[data-tail=\"hidden\"] > ." ++ bubbleClass ++ "::before, "
                    ++ scope
                    ++ "[data-tail=\"hidden\"] > ."
                    ++ bubbleClass
                    ++ "::after"
                )
                [ Css.property "content" "none" ]

        -- Outline ("::before") and fill ("::after") rules, per side.
        --
        -- The tail is a 0×0 pseudo-element with all-transparent borders
        -- except the one that points TOWARD the bubble — that's the
        -- visible triangle. We anchor the pseudo-element to the bubble
        -- edge nearest the trigger (e.g. for a `top` tooltip, anchor to
        -- the bubble's bottom edge → tail visible below).
        sideRules pos =
            let
                ( visibleBorderProp, anchorProp ) =
                    case pos of
                        Top ->
                            ( "border-top-color", "top" )

                        Bottom ->
                            ( "border-bottom-color", "bottom" )

                        Left ->
                            ( "border-left-color", "left" )

                        Right ->
                            ( "border-right-color", "right" )

                outlineSelector =
                    bubble pos ++ "::before"

                fillSelector =
                    bubble pos ++ "::after"
            in
            [ Css.Global.selector outlineSelector
                (tailBase
                    ++ [ Css.property visibleBorderProp (cssColor outlineColor)
                       , Css.property "border-width"
                            (String.fromFloat (tailSize + 1) ++ "px")
                       , Css.property anchorProp "100%"
                       ]
                )
            , Css.Global.selector fillSelector
                (tailBase
                    ++ [ Css.property visibleBorderProp (cssColor tooltipColor)
                       , Css.property "border-width"
                            (String.fromFloat tailSize ++ "px")
                       , Css.property anchorProp "100%"
                       ]
                )
            ]

        -- Cross-axis position: where along the tooltip edge the tail sits.
        -- For top/bottom tooltips the cross axis is X; for left/right it's Y.
        crossAxisRule pos a =
            let
                isHorizontal =
                    pos == Top || pos == Bottom

                ( startProp, endProp, transformProp ) =
                    if isHorizontal then
                        ( "left", "right", "translateX(-50%)" )

                    else
                        ( "top", "bottom", "translateY(-50%)" )

                styles =
                    case a of
                        AlignStart ->
                            [ Css.property startProp (String.fromFloat (tailSize + 8) ++ "px")
                            , Css.property endProp "auto"
                            , Css.property "transform" "none"
                            ]

                        AlignMiddle ->
                            [ Css.property startProp "50%"
                            , Css.property endProp "auto"
                            , Css.property "transform" transformProp
                            ]

                        AlignEnd ->
                            [ Css.property startProp "auto"
                            , Css.property endProp (String.fromFloat (tailSize + 8) ++ "px")
                            , Css.property "transform" "none"
                            ]

                positionAlignSelector =
                    scope
                        ++ "[data-position=\""
                        ++ positionToString pos
                        ++ "\"][data-align=\""
                        ++ alignToString a
                        ++ "\"] > ."
                        ++ bubbleClass
            in
            [ Css.Global.selector (positionAlignSelector ++ "::before") styles
            , Css.Global.selector (positionAlignSelector ++ "::after") styles
            ]
    in
    List.concatMap sideRules [ Top, Bottom, Left, Right ]
        ++ List.concatMap (\pos -> List.concatMap (crossAxisRule pos) [ AlignStart, AlignMiddle, AlignEnd ])
            [ Top, Bottom, Left, Right ]
        ++ [ tailHidden ]


cssColor : Color -> String
cssColor c =
    Nri.Ui.Colors.Extra.toCssString c



-- COLORS


tooltipColor : Color
tooltipColor =
    Colors.navy


outlineColor : Color
outlineColor =
    Nri.Ui.Colors.Extra.withAlpha 0.4 Colors.white
