module Nri.Ui.Tooltip.V3 exposing
    ( view, viewToggleTip
    , Attribute
    , paragraph, plaintext, markdown, html
    , withoutTail
    , onTop, onBottom, onLeft, onRight
    , onTopForQuizEngineMobile, onBottomForQuizEngineMobile, onLeftForQuizEngineMobile, onRightForQuizEngineMobile
    , onTopForNarrowMobile, onBottomForNarrowMobile, onLeftForNarrowMobile, onRightForNarrowMobile
    , onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
    , alignStart, alignMiddle, alignEnd
    , alignStartForQuizEngineMobile, alignMiddleForQuizEngineMobile, alignEndForQuizEngineMobile
    , alignStartForNarrowMobile, alignMiddleForNarrowMobile, alignEndForNarrowMobile
    , alignStartForMobile, alignMiddleForMobile, alignEndForMobile
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , onToggle, onTriggerKeyDown, stopTooltipMousePropagation
    , open
    , css, notMobileCss, mobileCss, quizEngineMobileCss, narrowMobileCss, containerCss
    , custom
    , nriDescription, testId
    , primaryLabel, auxiliaryDescription, helpfullyDisabled, disclosure
    )

{-| Patch changes:

  - defaults mobile-specific alignment and direction to the non-mobile version, rather than top and middle
  - adds onTopForQuizEngineMobile, onBottomForQuizEngineMobile, onLeftForQuizEngineMobile, onRightForQuizEngineMobile
  - adds onTopForNarrowMobile, onBottomForNarrowMobile, onLeftForNarrowMobile, onRightForNarrowMobile
  - adds alignStartForQuizEngineMobile, alignMiddleForQuizEngineMobile, alignEndForQuizEngineMobile
  - adds alignStartForNarrowMobile, alignMiddleForNarrowMobile, alignEndForNarrowMobile
  - adds narrowMobileCss
  - use internal `Content` module
  - adds `paragraph` and `markdown` support
  - add partially-transparent white border around tooltips
  - Use Nri.Ui.WhenFocusLeaves.V2
  - prevent default and stop propagation on click for disclosure tooltips
  - adds `helpfullyDisabled` option
  - adds `onTriggerKeyDown` option
  - add `stopTooltipMousePropagation` option

Changes from V2:

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
  - onHover -> onToggle

These tooltips aim to follow the accessibility recommendations from:

  - <https://inclusive-components.design/tooltips-toggletips>
  - <https://sarahmhigley.com/writing/tooltips-in-wcag-21/>

@docs view, viewToggleTip
@docs Attribute
@docs paragraph, plaintext, markdown, html
@docs withoutTail

@docs onTop, onBottom, onLeft, onRight
@docs onTopForQuizEngineMobile, onBottomForQuizEngineMobile, onLeftForQuizEngineMobile, onRightForQuizEngineMobile
@docs onTopForNarrowMobile, onBottomForNarrowMobile, onLeftForNarrowMobile, onRightForNarrowMobile
@docs onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile

@docs alignStart, alignMiddle, alignEnd
@docs alignStartForQuizEngineMobile, alignMiddleForQuizEngineMobile, alignEndForQuizEngineMobile
@docs alignStartForNarrowMobile, alignMiddleForNarrowMobile, alignEndForNarrowMobile
@docs alignStartForMobile, alignMiddleForMobile, alignEndForMobile

@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs onToggle, onTriggerKeyDown, stopTooltipMousePropagation
@docs open
@docs css, notMobileCss, mobileCss, quizEngineMobileCss, narrowMobileCss, containerCss
@docs custom
@docs nriDescription, testId
@docs primaryLabel, auxiliaryDescription, helpfullyDisabled, disclosure

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Content
import Css exposing (Color, Px, Style)
import Css.Global as Global
import EventExtras as Events
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import Maybe.Extra as Maybe
import Nri.Ui
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V2 as MediaQuery exposing (MediaQuery)
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.UiIcon.V1 as UiIcon
import Nri.Ui.WhenFocusLeaves.V2 as WhenFocusLeaves


{-| -}
type Attribute msg
    = Attribute (Tooltip msg -> Tooltip msg)


type alias Tooltip msg =
    { direction : Direction
    , alignment : Alignment
    , mobileDirection : Maybe Direction
    , mobileAlignment : Maybe Alignment
    , quizEngineMobileDirection : Maybe Direction
    , quizEngineMobileAlignment : Maybe Alignment
    , narrowMobileDirection : Maybe Direction
    , narrowMobileAlignment : Maybe Alignment
    , tail : Tail
    , content : List (Html msg)
    , attributes : List (Html.Attribute Never)
    , containerStyles : List Style
    , tooltipStyleOverrides : List Style
    , responsiveTooltipStyleOverrides : List MediaQuery
    , width : Width
    , padding : Padding
    , trigger : Maybe (Trigger msg)
    , triggerAttributes : List (Html.Attribute msg)
    , triggerKeyDownEvents : List (Key.Event msg)
    , stopTooltipMousePropagation : Maybe msg
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
            , mobileDirection = Nothing
            , mobileAlignment = Nothing
            , quizEngineMobileDirection = Nothing
            , quizEngineMobileAlignment = Nothing
            , narrowMobileDirection = Nothing
            , narrowMobileAlignment = Nothing
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
            , responsiveTooltipStyleOverrides = []
            , width = Exactly 320
            , padding = NormalPadding
            , trigger = Nothing
            , triggerAttributes = []
            , triggerKeyDownEvents = []
            , stopTooltipMousePropagation = Nothing
            , purpose = PrimaryLabel
            , isOpen = False
            }
    in
    List.foldl (\(Attribute applyAttr) acc -> applyAttr acc) defaultTooltip


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a plain-text string that will be put into a paragraph tag, with the default margin removed.
-}
paragraph : String -> Attribute msg
paragraph =
    Attribute << Content.paragraph


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown =
    Attribute << Content.markdown


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html


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
    Attribute (\config -> { config | mobileAlignment = Just alignment })


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


{-| Put the tail at the "middle" of the tooltip when the viewport has a mobile width.

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


withQuizEngineMobileAligment : Alignment -> Attribute msg
withQuizEngineMobileAligment alignment =
    Attribute (\config -> { config | quizEngineMobileAlignment = Just alignment })


{-| Put the tail at the "start" of the tooltip when the viewport has a quiz engine mobile (750px) width or narrower.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

-}
alignStartForQuizEngineMobile : Px -> Attribute msg
alignStartForQuizEngineMobile position =
    withQuizEngineMobileAligment (Start position)


{-| Put the tail at the "middle" of the tooltip when the viewport has a quiz engine mobile (750px) width or narrower.

     __________
    |___  ____|
        \/

-}
alignMiddleForQuizEngineMobile : Attribute msg
alignMiddleForQuizEngineMobile =
    withQuizEngineMobileAligment Middle


{-| Put the tail at the "end" of the tooltip when the viewport has a quiz engine mobile (750px) width or narrower.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

-}
alignEndForQuizEngineMobile : Px -> Attribute msg
alignEndForQuizEngineMobile position =
    withQuizEngineMobileAligment (End position)


withNarrowMobileAligment : Alignment -> Attribute msg
withNarrowMobileAligment alignment =
    Attribute (\config -> { config | narrowMobileAlignment = Just alignment })


{-| Put the tail at the "start" of the tooltip when the viewport has a narrow mobile (500px) width or narrower.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

-}
alignStartForNarrowMobile : Px -> Attribute msg
alignStartForNarrowMobile position =
    withNarrowMobileAligment (Start position)


{-| Put the tail at the "middle" of the tooltip when the viewport has a narrow mobile (500px) width or narrower.

     __________
    |___  ____|
        \/

-}
alignMiddleForNarrowMobile : Attribute msg
alignMiddleForNarrowMobile =
    withNarrowMobileAligment Middle


{-| Put the tail at the "end" of the tooltip when the viewport has a narrow mobile (500px) width or narrower.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

-}
alignEndForNarrowMobile : Px -> Attribute msg
alignEndForNarrowMobile position =
    withQuizEngineMobileAligment (End position)


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
    Attribute (\config -> { config | mobileDirection = Just direction })


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


withPositionForQuizEngineMobile : Direction -> Attribute msg
withPositionForQuizEngineMobile direction =
    Attribute (\config -> { config | quizEngineMobileDirection = Just direction })


{-| Set the position of the tooltip when the quiz engine breakpoint (750px) applies.

     __________
    |         |
    |___  ____|
        \/

-}
onTopForQuizEngineMobile : Attribute msg
onTopForQuizEngineMobile =
    withPositionForQuizEngineMobile OnTop


{-| Set the position of the tooltip when the quiz engine breakpoint (750px) applies.

      __________
     |         |
    <          |
     |_________|

-}
onRightForQuizEngineMobile : Attribute msg
onRightForQuizEngineMobile =
    withPositionForQuizEngineMobile OnRight


{-| Set the position of the tooltip when the quiz engine breakpoint (750px) applies.

     ___/\_____
    |         |
    |_________|

-}
onBottomForQuizEngineMobile : Attribute msg
onBottomForQuizEngineMobile =
    withPositionForQuizEngineMobile OnBottom


{-| Set the position of the tooltip when the quiz engine breakpoint (750px) applies.

      __________
     |         |
     |          >
     |_________|

-}
onLeftForQuizEngineMobile : Attribute msg
onLeftForQuizEngineMobile =
    withPositionForQuizEngineMobile OnLeft


withPositionForNarrowMobile : Direction -> Attribute msg
withPositionForNarrowMobile direction =
    Attribute (\config -> { config | narrowMobileDirection = Just direction })


{-| Set the position of the tooltip when the narrow mobile breakpoint (500px) applies.

     __________
    |         |
    |___  ____|
        \/

-}
onTopForNarrowMobile : Attribute msg
onTopForNarrowMobile =
    withPositionForNarrowMobile OnTop


{-| Set the position of the tooltip when the narrow mobile breakpoint (500px) applies.

      __________
     |         |
    <          |
     |_________|

-}
onRightForNarrowMobile : Attribute msg
onRightForNarrowMobile =
    withPositionForNarrowMobile OnRight


{-| Set the position of the tooltip when the narrow mobile breakpoint (500px) applies.

     ___/\_____
    |         |
    |_________|

-}
onBottomForNarrowMobile : Attribute msg
onBottomForNarrowMobile =
    withPositionForNarrowMobile OnBottom


{-| Set the position of the tooltip when the narrow mobile breakpoint (500px) applies.

      __________
     |         |
     |          >
     |_________|

-}
onLeftForNarrowMobile : Attribute msg
onLeftForNarrowMobile =
    withPositionForQuizEngineMobile OnLeft


{-| Set some custom styles on the tooltip.
-}
css : List Style -> Attribute msg
css tooltipStyleOverrides =
    Attribute (\config -> { config | tooltipStyleOverrides = config.tooltipStyleOverrides ++ tooltipStyleOverrides })


{-| Set some conditional custom styles on the tooltip according to a media query.
-}
responsiveCss : List MediaQuery -> Attribute msg
responsiveCss styles =
    -- Don't do the `MediaQuery.toStyles` here, because we want calls to multiple
    -- `responsiveCss` to be additive and processed by MediaQuery together.
    Attribute (\config -> { config | responsiveTooltipStyleOverrides = config.responsiveTooltipStyleOverrides ++ styles })


{-| Set styles that will only apply if the viewport is wider than NRI's mobile breakpoint.

Equivalent to:

    Tooltip.responsiveCss [ Nri.Ui.MediaQuery.V2.not Nri.Ui.MediaQuery.V2.mobile styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    responsiveCss [ MediaQuery.not MediaQuery.mobile styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's mobile breakpoint.

Equivalent to:

    Tooltip.responsiveCss [ Nri.Ui.MediaQuery.V2.mobile styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    responsiveCss [ MediaQuery.mobile styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's quiz-engine-specific mobile breakpoint.

Equivalent to:

    Tooltip.responsiveCss [ Nri.Ui.MediaQuery.V2.quizEngineMobile styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    responsiveCss [ MediaQuery.quizEngineMobile styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's narrow mobile breakpoint.

Equivalent to:

    Tooltip.responsiveCss [ Nri.Ui.MediaQuery.V2.narrowMobile styles ]

-}
narrowMobileCss : List Style -> Attribute msg
narrowMobileCss styles =
    responsiveCss [ MediaQuery.narrowMobile styles ]


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


{-| The Tooltip event cycle depends on whether you're following the Disclosure pattern, but disguising the Disclosure as a tooltip visually or you're actually adding a hint or label for sighted users.

If you're adding a tooltip to an element that _does_ something on its own, e.g., a "Print" ClickableSvg, then it doesn't make sense for the tooltip to change state on click/enter/space.

However, if you're adding a tooltip to an element that is not interactive at all if you don't count the tooltip, then we can use the click/enter/space events to manage the tooltip state too. This style of "tooltip" is the only kind that will be accessible for touch users on mobile -- it's important to get the access pattern right!

If the tooltip behavior you're seeing doesn't _feel_ quite right, consider whether you need to change tooltip "types" to `disclosure` or to `auxiliaryDescription`.

-}
onToggle : (Bool -> msg) -> Attribute msg
onToggle msg =
    Attribute (\config -> { config | trigger = Just (OnHover msg) })


{-| Add additional keydown handlers to the trigger element.

This is required rather than applying them directly to the trigger element because the attributes
passed into the trigger view function (to be applied to the trigger) include an onKeyDown event
handler that is used to close the tooltip when the escape key is pressed, and Elm requires that
only one onKeyDown event handler be applied to an element (otherwise, the last one wins).

-}
onTriggerKeyDown : List (Key.Event msg) -> Attribute msg
onTriggerKeyDown keyEvents =
    Attribute (\config -> { config | triggerKeyDownEvents = keyEvents })


{-| Stops propagation of mouseup / mousedown / click on the tooltip bubble.

Use this if your tooltip is contained withing a clickable/draggable element and you do not
want clicking on the tooltip bubble to act like a click on the parent.

-}
stopTooltipMousePropagation : msg -> Attribute msg
stopTooltipMousePropagation noopMsg =
    Attribute (\config -> { config | stopTooltipMousePropagation = Just noopMsg })


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


{-| Used when the content of the tooltip provides an "auxiliary description" for its content.

An auxiliary description is used when the tooltip content provides supplementary information about its trigger content.

-}
auxiliaryDescription : Attribute msg
auxiliaryDescription =
    Attribute (\config -> { config | purpose = AuxillaryDescription })


{-| Used when the tooltip trigger is disabled.

Provides information about why the tooltip trigger is disabled.

-}
helpfullyDisabled : Attribute msg
helpfullyDisabled =
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

This is a helper for setting up a commonly-used `disclosure` tooltip. Please see the documentation for `disclosure` to learn more.

-}
viewToggleTip : { label : String, lastId : Maybe String } -> List (Attribute msg) -> Html msg
viewToggleTip { label, lastId } attributes_ =
    let
        id =
            ExtraAttributes.safeId label

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
                              , WhenFocusLeaves.onKeyDown []
                                    { firstIds = [ triggerId ]
                                    , lastIds = [ Maybe.withDefault triggerId lastId ]
                                    , tabBackAction = msg False
                                    , tabForwardAction = msg False
                                    }
                              ]
                            , [ Events.onClickPreventDefaultAndStopPropagation (msg (not tooltip.isOpen))
                              , Key.onKeyDown (Key.escape (msg False) :: tooltip.triggerKeyDownEvents)
                              ]
                            )

                        _ ->
                            ( [ Events.onMouseEnter (msg True)
                              , Events.onMouseLeave (msg False)
                              ]
                            , [ Events.onFocus (msg True)
                              , Events.onBlur (msg False)
                              , Key.onKeyDown (Key.escape (msg False) :: tooltip.triggerKeyDownEvents)
                              ]
                            )

                Nothing ->
                    ( [], [ Key.onKeyDown tooltip.triggerKeyDownEvents ] )
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
                        [ Aria.expanded tooltip.isOpen
                        , Aria.controls [ id ]
                        ]
                 )
                    ++ buttonEvents
                    ++ tooltip.triggerAttributes
                )
            ]
        , viewTooltip id tooltip
        ]


viewTooltip : String -> Tooltip msg -> Html msg
viewTooltip tooltipId config =
    let
        ( _, _, mediaQueries ) =
            List.foldl
                (\( mediaQuery, maybeNextDirection, maybeNextAlignment ) ( prevDirection, prevAlignment, acc ) ->
                    if Maybe.isJust maybeNextDirection || Maybe.isJust maybeNextAlignment then
                        let
                            ( direction, alignment ) =
                                ( Maybe.withDefault prevDirection maybeNextDirection, Maybe.withDefault prevAlignment maybeNextAlignment )
                        in
                        ( direction
                        , alignment
                        , { acc
                            | positionTooltip = mediaQuery (positionTooltip direction alignment) :: acc.positionTooltip
                            , hoverAreaForDirection = mediaQuery (hoverAreaForDirection direction) :: acc.hoverAreaForDirection
                            , positioning = mediaQuery (positioning direction alignment) :: acc.positioning
                            , applyTail = mediaQuery (applyTail direction) :: acc.applyTail
                          }
                        )

                    else
                        ( prevDirection, prevAlignment, acc )
                )
                ( config.direction
                , config.alignment
                , { positionTooltip = []
                  , hoverAreaForDirection = []
                  , positioning = []
                  , applyTail = []
                  }
                )
                [ ( MediaQuery.mobile, config.mobileDirection, config.mobileAlignment )
                , ( MediaQuery.quizEngineMobile, config.quizEngineMobileDirection, config.quizEngineMobileAlignment )
                , ( MediaQuery.narrowMobile, config.narrowMobileDirection, config.narrowMobileAlignment )
                ]

        applyTail direction =
            case config.tail of
                WithTail ->
                    tailForDirection direction

                WithoutTail ->
                    []
    in
    Html.div
        [ Attributes.css <|
            [ Css.position Css.absolute
            , Css.boxSizing Css.borderBox
            , if config.isOpen then
                Css.batch []

              else
                Css.display Css.none
            ]
                ++ positionTooltip config.direction config.alignment
                ++ MediaQuery.toStyles mediaQueries.positionTooltip
        , -- Used for tests, since the visibility is controlled via CSS, which elm-program-test cannot account for
          Attributes.attribute "data-tooltip-visible" <|
            if config.isOpen then
                "true"

            else
                "false"
        , -- If the tooltip is the "primary label" for the content, then we can trust that the content
          -- in the tooltip is redundant. For example, if we have a ClickableSvg "Print" button, the button will
          -- *already have* an accessible name. It is not helpful to have the "Print" read out twice.
          Aria.hidden (config.purpose == PrimaryLabel)
        ]
        [ Root.div
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
                 , Css.border3 (Css.px 1) Css.solid outlineColor
                 , Fonts.baseFont
                 , Css.fontSize (Css.px 15)
                 , Css.fontWeight (Css.int 600)
                 , Css.color Colors.white
                 , Shadows.high
                 , Global.descendants
                    [ Global.a
                        [ Css.color Colors.white
                        , Css.borderColor Colors.white
                        , Css.textDecoration Css.none
                        , Css.borderBottom3 (Css.px 1) Css.solid Colors.white
                        , Css.visited [ Css.color Colors.white ]
                        , Css.hover [ Css.color Colors.white ]
                        , Css.pseudoClass "focus-visible"
                            [ Css.outline3 (Css.px 2) Css.solid Css.transparent
                            , Css.property "box-shadow" "0 0 0 2px #FFF"
                            ]
                        ]
                    ]
                 ]
                    ++ positioning config.direction config.alignment
                    ++ applyTail config.direction
                    ++ MediaQuery.toStyles (mediaQueries.positioning ++ mediaQueries.applyTail)
                    ++ config.tooltipStyleOverrides
                )

             -- We need to keep this animation in tests to make it pass: check out
             -- the NoAnimations middleware. So if you change the name here, please
             -- change that as well
             , Attributes.class "dont-disable-animation"
             , Role.toolTip
             ]
                ++ List.map (Attributes.map never) config.attributes
                ++ (case config.stopTooltipMousePropagation of
                        Nothing ->
                            []

                        Just msg ->
                            -- Adding events to a div is generally "bad" which is why `Accessibility.Styled` does not allow it.
                            -- But in this case we only need to add events to stop them from propagating, so that feels fine in spirit.
                            [ Events.stopPropagationOn "mousedown" (Json.Decode.succeed ( msg, True ))
                            , Events.stopPropagationOn "mouseup" (Json.Decode.succeed ( msg, True ))
                            , Events.stopPropagationOn "click" (Json.Decode.succeed ( msg, True ))
                            ]
                   )
                ++ [ Attributes.id tooltipId ]
            )
            (config.content
                ++ [ Html.div
                        [ ExtraAttributes.nriDescription "tooltip-hover-bridge"
                        , Attributes.css
                            (Css.position Css.absolute
                                :: hoverAreaForDirection config.direction
                                ++ MediaQuery.toStyles mediaQueries.hoverAreaForDirection
                            )
                        ]
                        []
                   ]
            )
        ]


tailSize : Float
tailSize =
    8


tooltipColor : Color
tooltipColor =
    Colors.navy


outlineColor : Color
outlineColor =
    Css.rgba 255 255 255 0.5


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
                    [ Css.left customOffset, Css.right Css.unset ]

                Middle ->
                    [ Css.left (Css.pct 50), Css.right Css.unset ]

                End customOffset ->
                    [ Css.left Css.unset, Css.right customOffset ]

        topToBottomPosition =
            case alignment of
                Start customOffset ->
                    [ Css.top customOffset, Css.bottom Css.unset ]

                Middle ->
                    [ Css.top (Css.pct 50), Css.bottom Css.unset ]

                End customOffset ->
                    [ Css.top Css.unset, Css.bottom customOffset ]
    in
    case direction of
        OnTop ->
            Css.top (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2)) :: Css.bottom Css.unset :: ltrPosition

        OnBottom ->
            Css.top Css.unset :: Css.bottom (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2)) :: ltrPosition

        OnLeft ->
            Css.left (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2)) :: Css.right Css.unset :: topToBottomPosition

        OnRight ->
            Css.left Css.unset :: Css.right (Css.calc (Css.px (negate tailSize)) Css.minus (Css.px 2)) :: topToBottomPosition



-- TAILS


positioning : Direction -> Alignment -> List Style
positioning direction alignment =
    let
        topBottomAlignment =
            case alignment of
                Start _ ->
                    [ Css.left (Css.px offCenterOffset), Css.right Css.unset ]

                Middle ->
                    [ Css.left (Css.pct 50), Css.right Css.unset ]

                End _ ->
                    [ Css.left Css.unset, Css.right (Css.px offCenterOffset) ]

        rightLeftAlignment =
            case alignment of
                Start _ ->
                    [ Css.top (Css.calc (Css.px offCenterOffset) Css.minus (Css.px tailSize)), Css.bottom Css.unset ]

                Middle ->
                    [ Css.top (Css.calc (Css.pct 50) Css.minus (Css.px tailSize)), Css.bottom Css.unset ]

                End _ ->
                    [ Css.top Css.unset, Css.bottom (Css.calc (Css.px offCenterOffset) Css.minus (Css.px tailSize)) ]
    in
    case direction of
        OnTop ->
            [ Css.property "transform" "translate(-50%, -100%)"
            , getTailPositioning
                { xAlignment = topBottomAlignment
                , yAlignment = [ Css.top (Css.pct 100), Css.bottom Css.unset ]
                }
            ]

        OnBottom ->
            [ Css.property "transform" "translate(-50%, 0)"
            , getTailPositioning
                { xAlignment = topBottomAlignment
                , yAlignment = [ Css.top Css.unset, Css.bottom (Css.pct 100) ]
                }
            ]

        OnRight ->
            [ Css.property "transform" "translate(0, -50%)"
            , getTailPositioning
                { xAlignment = [ Css.left Css.unset, Css.right (Css.pct 100) ]
                , yAlignment = rightLeftAlignment
                }
            ]

        OnLeft ->
            [ Css.property "transform" "translate(-100%, -50%)"
            , getTailPositioning
                { xAlignment = [ Css.left (Css.pct 100), Css.right Css.unset ]
                , yAlignment = rightLeftAlignment
                }
            ]


tailForDirection : Direction -> List Style
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


hoverAreaForDirection : Direction -> List Style
hoverAreaForDirection direction =
    case direction of
        OnTop ->
            bottomHoverArea

        OnBottom ->
            topHoverArea

        OnLeft ->
            rightHoverArea

        OnRight ->
            leftHoverArea


topHoverArea : List Style
topHoverArea =
    [ Css.bottom (Css.pct 100)
    , Css.left Css.zero
    , Css.right Css.zero
    , Css.top Css.unset
    , Css.height (Css.px (tailSize + 3))
    ]


bottomHoverArea : List Style
bottomHoverArea =
    [ Css.top (Css.pct 100)
    , Css.left Css.zero
    , Css.right Css.zero
    , Css.bottom Css.unset
    , Css.height (Css.px (tailSize + 3))
    ]


leftHoverArea : List Style
leftHoverArea =
    [ Css.right (Css.pct 100)
    , Css.top Css.zero
    , Css.bottom Css.zero
    , Css.left Css.unset
    , Css.width (Css.px (tailSize + 3))
    ]


rightHoverArea : List Style
rightHoverArea =
    [ Css.left (Css.pct 100)
    , Css.top Css.zero
    , Css.bottom Css.zero
    , Css.right Css.unset
    , Css.width (Css.px (tailSize + 3))
    ]


bottomTail : List Style
bottomTail =
    [ Css.before
        [ Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
        , Css.borderColor4 outlineColor Css.transparent Css.transparent Css.transparent
        , Css.marginLeft (Css.px (-tailSize - 1))
        ]
    , Css.after
        [ Css.property "border-width" (String.fromFloat tailSize ++ "px")
        , Css.borderColor4 tooltipColor Css.transparent Css.transparent Css.transparent
        , Css.margin4 Css.zero Css.zero Css.zero (Css.px -tailSize)
        ]
    ]


topTail : List Style
topTail =
    [ Css.before
        [ Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
        , Css.borderColor4 Css.transparent Css.transparent outlineColor Css.transparent
        , Css.marginLeft (Css.px (-tailSize - 1))
        ]
    , Css.after
        [ Css.property "border-width" (String.fromFloat tailSize ++ "px")
        , Css.borderColor4 Css.transparent Css.transparent tooltipColor Css.transparent
        , Css.margin4 Css.zero Css.zero Css.zero (Css.px -tailSize)
        ]
    ]


rightTail : List Style
rightTail =
    [ Css.before
        [ Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
        , Css.borderColor4 Css.transparent Css.transparent Css.transparent outlineColor
        , Css.marginLeft Css.zero
        ]
    , Css.after
        [ Css.property "border-width" (String.fromFloat tailSize ++ "px")
        , Css.borderColor4 Css.transparent Css.transparent Css.transparent tooltipColor
        , Css.margin4 (Css.px 1) (Css.px 2) Css.zero Css.zero
        ]
    ]


leftTail : List Style
leftTail =
    [ Css.before
        [ Css.property "border-width" (String.fromFloat (tailSize + 1) ++ "px")
        , Css.borderColor4 Css.transparent outlineColor Css.transparent Css.transparent
        , Css.marginLeft Css.zero
        ]
    , Css.after
        [ Css.property "border-width" (String.fromFloat tailSize ++ "px")
        , Css.borderColor4 Css.transparent tooltipColor Css.transparent Css.transparent
        , Css.margin4 (Css.px 1) Css.zero Css.zero (Css.px 2)
        ]
    ]


getTailPositioning : { xAlignment : List Style, yAlignment : List Style } -> Style
getTailPositioning config =
    Css.batch
        [ Css.before (positionTail config)
        , Css.after (positionTail config)
        ]


positionTail : { xAlignment : List Style, yAlignment : List Style } -> List Style
positionTail { xAlignment, yAlignment } =
    [ Css.property "border" "solid transparent"
    , Css.property "content" "\" \""
    , Css.height Css.zero
    , Css.width Css.zero
    , Css.position Css.absolute
    , Css.pointerEvents Css.none
    ]
        ++ xAlignment
        ++ yAlignment
