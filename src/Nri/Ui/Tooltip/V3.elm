module Nri.Ui.Tooltip.V3 exposing
    ( view, viewToggleTip
    , Attribute
    , plaintext, html
    , withoutTail
    , onTop, onBottom, onLeft, onRight
    , onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
    , alignStart, alignMiddle, alignEnd
    , alignStartForMobile, alignMiddleForMobile, alignEndForMobile
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , onHover
    , open
    , css, notMobileCss, mobileCss, quizEngineMobileCss, containerCss
    , custom
    , nriDescription, testId
    , primaryLabel, auxiliaryDescription, disclosure
    )

{-| Changes from V2:

  - Support `disclosure` pattern for rich-content tooltips
  - render tooltip content in the DOM when closed (now, they're hidden with display:none)
  - tooltips MUST be closable via keyboard without moving focus. [Understanding Success Criterion 1.4.13: Content on Hover or Focus](https://www.w3.org/WAI/WCAG21/Understanding/content-on-hover-or-focus.html)
  - remove onClick helper
  - prefer the accessible name to using aria-labelledby and aria-label together
  - :skull: remove customTooltipAttributes
  - change `css` to extend the current list of styles, NOT override them entirely.
  - fix spelling of "auxillary" to "auxiliary"
  - toggleTip -> viewToggleTip
  - Adds notMobileCss, mobileCss, quizEngineMobileCss

These tooltips aim to follow the accessibility recommendations from:

  - <https://inclusive-components.design/tooltips-toggletips>
  - <https://sarahmhigley.com/writing/tooltips-in-wcag-21/>

@docs view, viewToggleTip
@docs Attribute
@docs plaintext, html
@docs withoutTail
@docs onTop, onBottom, onLeft, onRight
@docs onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
@docs alignStart, alignMiddle, alignEnd
@docs alignStartForMobile, alignMiddleForMobile, alignEndForMobile
@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs onHover
@docs open
@docs css, notMobileCss, mobileCss, quizEngineMobileCss, containerCss
@docs custom
@docs nriDescription, testId
@docs primaryLabel, auxiliaryDescription, disclosure

-}

import Accessibility.Styled as Html exposing (Attribute, Html, text)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (Color, Px, Style)
import Css.Global as Global
import Css.Media exposing (withMedia)
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.UiIcon.V1 as UiIcon
import Nri.Ui.WhenFocusLeaves.V1 as WhenFocusLeaves
import String.Extra


{-| -}
type Attribute msg
    = Attribute (Tooltip msg -> Tooltip msg)


type alias Tooltip msg =
    { direction : Direction
    , alignment : Alignment
    , mobileDirection : Direction
    , mobileAlignment : Alignment
    , tail : Tail
    , content : List (Html msg)
    , attributes : List (Html.Attribute Never)
    , containerStyles : List Style
    , tooltipStyleOverrides : List Style
    , width : Width
    , padding : Padding
    , trigger : Maybe (Trigger msg)
    , triggerAttributes : List (Html.Attribute msg)
    , purpose : Purpose
    , isOpen : Bool
    }


buildAttributes : List (Attribute msg) -> Tooltip msg
buildAttributes =
    let
        defaultTooltip : Tooltip msg
        defaultTooltip =
            { direction = OnTop
            , alignment = Middle
            , mobileDirection = OnTop
            , mobileAlignment = Middle
            , tail = WithTail
            , content = []
            , attributes = []
            , containerStyles =
                [ Css.boxSizing Css.borderBox
                , Css.display Css.inlineBlock
                , Css.textAlign Css.left
                , Css.position Css.relative
                ]
            , tooltipStyleOverrides = []
            , width = Exactly 320
            , padding = NormalPadding
            , trigger = Nothing
            , triggerAttributes = []
            , purpose = PrimaryLabel
            , isOpen = False
            }
    in
    List.foldl (\(Attribute applyAttr) acc -> applyAttr acc) defaultTooltip


{-| -}
plaintext : String -> Attribute msg
plaintext content =
    Attribute (\config -> { config | content = [ text content ] })


{-| -}
html : List (Html msg) -> Attribute msg
html content =
    Attribute (\config -> { config | content = content })


type Tail
    = WithTail
    | WithoutTail


{-| Where should the tail be positioned relative to the tooltip?
-}
type Alignment
    = Start Px
    | Middle
    | End Px


{-| Makes it so that the tooltip does not have a tail!
-}
withoutTail : Attribute msg
withoutTail =
    Attribute (\config -> { config | tail = WithoutTail })


withAligment : Alignment -> Attribute msg
withAligment alignment =
    Attribute (\config -> { config | alignment = alignment })


{-| Put the tail at the "start" of the tooltip.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

-}
alignStart : Px -> Attribute msg
alignStart position =
    withAligment (Start position)


{-| Put the tail at the "middle" of the tooltip. This is the default behavior.

     __________
    |___  ____|
        \/

-}
alignMiddle : Attribute msg
alignMiddle =
    withAligment Middle


{-| Put the tail at the "end" of the tooltip.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

-}
alignEnd : Px -> Attribute msg
alignEnd position =
    withAligment (End position)


withMobileAligment : Alignment -> Attribute msg
withMobileAligment alignment =
    Attribute (\config -> { config | mobileAlignment = alignment })


{-| Put the tail at the "start" of the tooltip when the viewport has a mobile width.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

-}
alignStartForMobile : Px -> Attribute msg
alignStartForMobile position =
    withMobileAligment (Start position)


{-| Put the tail at the "middle" of the tooltip when the viewport has a mobile width. This is the default behavior.

     __________
    |___  ____|
        \/

-}
alignMiddleForMobile : Attribute msg
alignMiddleForMobile =
    withMobileAligment Middle


{-| Put the tail at the "end" of the tooltip when the viewport has a mobile width.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

-}
alignEndForMobile : Px -> Attribute msg
alignEndForMobile position =
    withMobileAligment (End position)


{-| Where should this tooltip be positioned relative to the trigger?
-}
type Direction
    = OnTop
    | OnBottom
    | OnLeft
    | OnRight


withPosition : Direction -> Attribute msg
withPosition direction =
    Attribute (\config -> { config | direction = direction })


{-|

     __________
    |         |
    |___  ____|
        \/

-}
onTop : Attribute msg
onTop =
    withPosition OnTop


{-|

      __________
     |         |
    <          |
     |_________|

-}
onRight : Attribute msg
onRight =
    withPosition OnRight


{-|

     ___/\_____
    |         |
    |_________|

-}
onBottom : Attribute msg
onBottom =
    withPosition OnBottom


{-|

      __________
     |         |
     |          >
     |_________|

-}
onLeft : Attribute msg
onLeft =
    withPosition OnLeft


withPositionForMobile : Direction -> Attribute msg
withPositionForMobile direction =
    Attribute (\config -> { config | mobileDirection = direction })


{-| Set the position of the tooltip when the mobile breakpoint applies.

     __________
    |         |
    |___  ____|
        \/

-}
onTopForMobile : Attribute msg
onTopForMobile =
    withPositionForMobile OnTop


{-| Set the position of the tooltip when the mobile breakpoint applies.

      __________
     |         |
    <          |
     |_________|

-}
onRightForMobile : Attribute msg
onRightForMobile =
    withPositionForMobile OnRight


{-| Set the position of the tooltip when the mobile breakpoint applies.

     ___/\_____
    |         |
    |_________|

-}
onBottomForMobile : Attribute msg
onBottomForMobile =
    withPositionForMobile OnBottom


{-| Set the position of the tooltip when the mobile breakpoint applies.

      __________
     |         |
     |          >
     |_________|

-}
onLeftForMobile : Attribute msg
onLeftForMobile =
    withPositionForMobile OnLeft


{-| Set some custom styles on the tooltip.
-}
css : List Style -> Attribute msg
css tooltipStyleOverrides =
    Attribute (\config -> { config | tooltipStyleOverrides = config.tooltipStyleOverrides ++ tooltipStyleOverrides })


{-| Set styles that will only apply if the viewport is wider than NRI's mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's quiz-engine-specific mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute msg
custom attributes =
    Attribute (\config -> { config | attributes = config.attributes ++ attributes })


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
containerCss : List Style -> Attribute msg
containerCss styles =
    Attribute (\config -> { config | containerStyles = config.containerStyles ++ styles })


{-| Should the tooltip be exactly some measurement or fit to the width of the
content?
-}
type Width
    = Exactly Int
    | FitToContent


withWidth : Width -> Attribute msg
withWidth width =
    Attribute (\config -> { config | width = width })


{-| Define a size in `px` for the tooltips's total width. The default is 320px.
-}
exactWidth : Int -> Attribute msg
exactWidth width =
    withWidth (Exactly width)


{-| Tooltip width fits its content.
-}
fitToContent : Attribute msg
fitToContent =
    withWidth FitToContent


{-| How much padding should be around the content inside the tooltip?
-}
type Padding
    = SmallPadding
    | NormalPadding
    | CustomPadding Float


paddingToStyle : Padding -> Style
paddingToStyle padding =
    case padding of
        SmallPadding ->
            Css.padding2 (Css.px 10) (Css.px 13)

        NormalPadding ->
            Css.padding (Css.px 20)

        CustomPadding padding_ ->
            Css.padding (Css.px padding_)


withPadding : Padding -> Attribute msg
withPadding padding =
    Attribute (\config -> { config | padding = padding })


{-| -}
smallPadding : Attribute msg
smallPadding =
    withPadding SmallPadding


{-| This the default spacing.
-}
normalPadding : Attribute msg
normalPadding =
    withPadding NormalPadding


{-| Pass in the desired spacing around the edge of the tooltip (pixels).
-}
customPadding : Float -> Attribute msg
customPadding value =
    withPadding (CustomPadding value)


type Trigger msg
    = OnHover (Bool -> msg)


{-| The tooltip opens when hovering over the trigger element, and closes when the hover stops.
-}
onHover : (Bool -> msg) -> Attribute msg
onHover msg =
    Attribute (\config -> { config | trigger = Just (OnHover msg) })


type Purpose
    = PrimaryLabel
    | AuxillaryDescription
    | Disclosure { triggerId : String, lastId : Maybe String }


{-| Used when the content of the tooltip is identical to the accessible name.

For example, when using the Tooltip component with the ClickableSvg component, the Tooltip is providing
extra information to sighted users that screenreader users already have.

This is the default.

-}
primaryLabel : Attribute msg
primaryLabel =
    Attribute (\config -> { config | purpose = PrimaryLabel })


{-| Used when the content of the tooltip provides an "auxillary description" for its content.

An auxillary description is used when the tooltip content provides supplementary information about its trigger content
e.g. when the trigger content is a word in the middle of a body of text that requires additional explanation.

-}
auxiliaryDescription : Attribute msg
auxiliaryDescription =
    Attribute (\config -> { config | purpose = AuxillaryDescription })


{-| Sometimes a "tooltip" only _looks_ like a tooltip, but is really more about hiding and showing extra information when the user asks for it.

If clicking the "tooltip trigger" only ever shows you more info (and especially if this info is rich or interactable), use this attribute.

For more information, please read [Sarah Higley's "Tooltips in the time of WCAG 2.1" post](https://sarahmhigley.com/writing/tooltips-in-wcag-21).

You will need to pass in the last focusable element in the disclosed content in order for:

  - any focusable elements in the disclosed content to be keyboard accessible
  - the disclosure to close appropriately when the user tabs past all of the disclosed content

You may pass a lastId of Nothing if there is NO focusable content within the disclosure.

-}
disclosure : { triggerId : String, lastId : Maybe String } -> Attribute msg
disclosure exitFocusManager =
    Attribute (\config -> { config | purpose = Disclosure exitFocusManager })


{-| Pass a bool indicating whether the tooltip should be open or closed.
-}
open : Bool -> Attribute msg
open isOpen =
    Attribute (\config -> { config | isOpen = isOpen })


{-| Here's what the fields in the configuration record do:

  - `trigger`: What element do you interact with to open the tooltip?
  - `id`: A unique identifier used to associate the trigger with its content

-}
view :
    { trigger : List (Html.Attribute msg) -> Html msg
    , id : String -- Accessibility: Used to match tooltip to trigger
    }
    -> List (Attribute msg)
    -> Html msg
view config attributes =
    viewTooltip_ config (buildAttributes attributes)


{-| Supplementary information triggered by a "?" icon.
-}
viewToggleTip : { label : String, lastId : Maybe String } -> List (Attribute msg) -> Html msg
viewToggleTip { label, lastId } attributes_ =
    let
        id =
            String.Extra.dasherize label

        triggerId =
            "tooltip-trigger__" ++ id
    in
    view
        { trigger =
            \events ->
                ClickableSvg.button label
                    UiIcon.help
                    [ ClickableSvg.exactWidth 20
                    , ClickableSvg.exactHeight 20
                    , ClickableSvg.custom events
                    , ClickableSvg.id triggerId
                    , ClickableSvg.css
                        [ -- Take up enough room within the document flow
                          Css.margin (Css.px 5)
                        ]
                    ]
        , id = id
        }
        (custom
            [ Attributes.class "Nri-Ui-Tooltip-V2-ToggleTip"
            , Attributes.id id
            ]
            :: disclosure { triggerId = triggerId, lastId = lastId }
            :: attributes_
        )



-- INTERNALS


viewTooltip_ :
    { trigger : List (Html.Attribute msg) -> Html msg
    , id : String -- Accessibility: Used to match tooltip to trigger
    }
    -> Tooltip msg
    -> Html msg
viewTooltip_ { trigger, id } tooltip =
    let
        ( containerEvents, buttonEvents ) =
            case tooltip.trigger of
                Just (OnHover msg) ->
                    case tooltip.purpose of
                        Disclosure { triggerId, lastId } ->
                            ( [ Events.onMouseEnter (msg True)
                              , Events.onMouseLeave (msg False)
                              , WhenFocusLeaves.toAttribute
                                    { firstId = triggerId
                                    , lastId = Maybe.withDefault triggerId lastId
                                    , tabBackAction = msg False
                                    , tabForwardAction = msg False
                                    }
                              ]
                            , [ Events.onClick (msg (not tooltip.isOpen))
                              , Key.onKeyDown [ Key.escape (msg False) ]
                              ]
                            )

                        _ ->
                            ( [ Events.onMouseEnter (msg True)
                              , Events.onMouseLeave (msg False)
                              ]
                            , [ Events.onFocus (msg True)
                              , Events.onBlur (msg False)
                              , Key.onKeyDown [ Key.escape (msg False) ]
                              ]
                            )

                Nothing ->
                    ( [], [] )
    in
    Nri.Ui.styled Root.div
        "Nri-Ui-Tooltip-V2"
        tooltip.containerStyles
        containerEvents
        [ Html.div
            [ Attributes.css
                [ -- using display flex not so that the wrapping div fits its
                  -- contents exactly. otherwise, it will be at least of the
                  -- size of a line of text, adding some extra vertical space
                  -- when the trigger is short (making it look like vertical
                  -- alignment is broken). if you ever need to change this back
                  -- to `block` or `inline-block`, consider adjusting the
                  -- `font-size` to zero to achieve a similar effect.
                  Css.displayFlex
                ]
            ]
            [ trigger
                ((case tooltip.purpose of
                    PrimaryLabel ->
                        [-- The content should already have an accessible name.
                        ]

                    AuxillaryDescription ->
                        [ Aria.describedBy [ id ] ]

                    Disclosure _ ->
                        [ Widget.expanded tooltip.isOpen
                        , Aria.controls id
                        ]
                 )
                    ++ buttonEvents
                    ++ tooltip.triggerAttributes
                )
            , hoverBridge tooltip
            ]

        -- Popout is rendered after the overlay, to allow client code to give it
        -- priority when clicking by setting its position
        , viewTooltip id tooltip
        ]


{-| This is a "bridge" for the cursor to move from trigger content to tooltip, so the user can click on links, etc.
-}
hoverBridge : Tooltip msg -> Html msg
hoverBridge { isOpen, direction } =
    let
        bridgeLength =
            tailSize + 5
    in
    if isOpen then
        Nri.Ui.styled Html.div
            "tooltip-hover-bridge"
            [ Css.boxSizing Css.borderBox
            , Css.padding (Css.px tailSize)
            , Css.position Css.absolute
            , Css.batch <|
                case direction of
                    OnTop ->
                        [ Css.top (Css.px -bridgeLength)
                        , Css.left Css.zero
                        , Css.width (Css.pct 100)
                        , Css.height (Css.px tailSize)
                        ]

                    OnRight ->
                        [ Css.right (Css.px -bridgeLength)
                        , Css.top Css.zero
                        , Css.width (Css.px tailSize)
                        , Css.height (Css.pct 100)
                        ]

                    OnBottom ->
                        [ Css.bottom (Css.px -bridgeLength)
                        , Css.left Css.zero
                        , Css.width (Css.pct 100)
                        , Css.height (Css.px tailSize)
                        ]

                    OnLeft ->
                        [ Css.left (Css.px -bridgeLength)
                        , Css.top Css.zero
                        , Css.width (Css.px tailSize)
                        , Css.height (Css.pct 100)
                        ]
            ]
            []
            []

    else
        text ""


viewTooltip : String -> Tooltip msg -> Html msg
viewTooltip tooltipId config =
    Html.div
        [ Attributes.css
            [ Css.position Css.absolute
            , Css.Media.withMedia [ MediaQuery.notMobile ]
                (positionTooltip config.direction config.alignment)
            , Css.Media.withMedia [ MediaQuery.mobile ]
                (positionTooltip config.mobileDirection config.mobileAlignment)
            , Css.boxSizing Css.borderBox
            , if config.isOpen then
                Css.batch []

              else
                Css.display Css.none
            ]
        ]
        [ Html.div
            ([ Attributes.css
                ([ Css.boxSizing Css.borderBox
                 , Css.borderRadius (Css.px 8)
                 , case config.width of
                    Exactly width ->
                        Css.width (Css.px (toFloat width))

                    FitToContent ->
                        Css.whiteSpace Css.noWrap
                 , paddingToStyle config.padding
                 , Css.position Css.absolute
                 , Css.zIndex (Css.int 100)
                 , Css.backgroundColor Colors.navy
                 , Css.border3 (Css.px 1) Css.solid Colors.navy
                 , Css.Media.withMedia [ MediaQuery.notMobile ]
                    [ positioning config.direction config.alignment
                    , case config.tail of
                        WithTail ->
                            tailForDirection config.direction

                        WithoutTail ->
                            Css.batch []
                    ]
                 , Css.Media.withMedia [ MediaQuery.mobile ]
                    [ positioning config.mobileDirection config.mobileAlignment
                    , case config.tail of
                        WithTail ->
                            tailForDirection config.mobileDirection

                        WithoutTail ->
                            Css.batch []
                    ]
                 , Fonts.baseFont
                 , Css.fontSize (Css.px 16)
                 , Css.fontWeight (Css.int 600)
                 , Css.color Colors.white
                 , Shadows.high
                 , Global.descendants [ Global.a [ Css.textDecoration Css.underline ] ]
                 , Global.descendants [ Global.a [ Css.color Colors.white ] ]
                 ]
                    ++ config.tooltipStyleOverrides
                )

             -- We need to keep this animation in tests to make it pass: check out
             -- the NoAnimations middleware. So if you change the name here, please
             -- change that as well
             , Attributes.class "dont-disable-animation"
             , Role.toolTip
             ]
                ++ config.attributes
                ++ [ Attributes.id tooltipId ]
            )
            config.content
        ]


tailSize : Float
tailSize =
    8


tooltipColor : Color
tooltipColor =
    Colors.navy


offCenterOffset : Float
offCenterOffset =
    20


{-| This returns absolute positioning styles for the popout container for a given tail position.
-}
positionTooltip : Direction -> Alignment -> List Style
positionTooltip direction alignment =
    let
        ltrPosition =
            case alignment of
                Start customOffset ->
                    Css.left customOffset

                Middle ->
                    Css.left (Css.pct 50)

                End customOffset ->
                    Css.right customOffset

        topToBottomPosition =
            case alignment of
                Start customOffset ->
                    Css.top customOffset

                Middle ->
                    Css.top (Css.pct 50)

                End customOffset ->
                    Css.bottom customOffset
    in
    case direction of
        OnTop ->
            [ ltrPosition
            , Css.top (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2))
            ]

        OnBottom ->
            [ ltrPosition
            , Css.bottom (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2))
            ]

        OnLeft ->
            [ topToBottomPosition
            , Css.left (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2))
            ]

        OnRight ->
            [ topToBottomPosition
            , Css.right (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2))
            ]



-- TAILS


positioning : Direction -> Alignment -> Style
positioning direction alignment =
    let
        topBottomAlignment =
            case alignment of
                Start _ ->
                    Css.left (Css.px offCenterOffset)

                Middle ->
                    Css.left (Css.pct 50)

                End _ ->
                    Css.right (Css.px offCenterOffset)

        rightLeftAlignment =
            case alignment of
                Start _ ->
                    Css.property "top" ("calc(-" ++ String.fromFloat tailSize ++ "px + " ++ String.fromFloat offCenterOffset ++ "px)")

                Middle ->
                    Css.property "top" ("calc(-" ++ String.fromFloat tailSize ++ "px + 50%)")

                End _ ->
                    Css.property "bottom" ("calc(-" ++ String.fromFloat tailSize ++ "px + " ++ String.fromFloat offCenterOffset ++ "px)")
    in
    case direction of
        OnTop ->
            Css.batch
                [ Css.property "transform" "translate(-50%, -100%)"
                , getTailPositioning
                    { xAlignment = topBottomAlignment
                    , yAlignment = Css.top (Css.pct 100)
                    }
                ]

        OnBottom ->
            Css.batch
                [ Css.property "transform" "translate(-50%, 0)"
                , getTailPositioning
                    { xAlignment = topBottomAlignment
                    , yAlignment = Css.bottom (Css.pct 100)
                    }
                ]

        OnRight ->
            Css.batch
                [ Css.property "transform" "translate(0, -50%)"
                , getTailPositioning
                    { xAlignment = Css.right (Css.pct 100)
                    , yAlignment = rightLeftAlignment
                    }
                ]

        OnLeft ->
            Css.batch
                [ Css.property "transform" "translate(-100%, -50%)"
                , getTailPositioning
                    { xAlignment = Css.left (Css.pct 100)
                    , yAlignment = rightLeftAlignment
                    }
                ]


tailForDirection : Direction -> Style
tailForDirection direction =
    case direction of
        OnTop ->
            bottomTail

        OnBottom ->
            topTail

        OnRight ->
            leftTail

        OnLeft ->
            rightTail


bottomTail : Style
bottomTail =
    Css.batch
        [ Css.before
            [ Css.borderTopColor tooltipColor
            , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
            , Css.marginLeft (Css.px (-tailSize - 1))
            ]
        , Css.after
            [ Css.borderTopColor tooltipColor
            , Css.property "border-width" (String.fromFloat tailSize ++ "px")
            , Css.marginLeft (Css.px -tailSize)
            ]
        ]


topTail : Style
topTail =
    Css.batch
        [ Css.before
            [ Css.borderBottomColor tooltipColor
            , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
            , Css.marginLeft (Css.px (-tailSize - 1))
            ]
        , Css.after
            [ Css.borderBottomColor tooltipColor
            , Css.property "border-width" (String.fromFloat tailSize ++ "px")
            , Css.marginLeft (Css.px -tailSize)
            ]
        ]


rightTail : Style
rightTail =
    Css.batch
        [ Css.before
            [ Css.borderLeftColor tooltipColor
            , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
            ]
        , Css.after
            [ Css.borderLeftColor tooltipColor
            , Css.property "border-width" (String.fromFloat tailSize ++ "px")
            , Css.marginTop (Css.px 1)
            , Css.marginRight (Css.px 2)
            ]
        ]


leftTail : Style
leftTail =
    Css.batch
        [ Css.before
            [ Css.borderRightColor tooltipColor
            , Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
            ]
        , Css.after
            [ Css.borderRightColor tooltipColor
            , Css.property "border-width" (String.fromFloat tailSize ++ "px")
            , Css.marginTop (Css.px 1)
            , Css.marginLeft (Css.px 2)
            ]
        ]


getTailPositioning : { xAlignment : Style, yAlignment : Style } -> Style
getTailPositioning config =
    Css.batch
        [ Css.before (positionTail config)
        , Css.after (positionTail config)
        ]


positionTail : { xAlignment : Style, yAlignment : Style } -> List Style
positionTail { xAlignment, yAlignment } =
    [ xAlignment
    , yAlignment
    , Css.property "border" "solid transparent"
    , Css.property "content" "\" \""
    , Css.height Css.zero
    , Css.width Css.zero
    , Css.position Css.absolute
    , Css.pointerEvents Css.none
    ]
