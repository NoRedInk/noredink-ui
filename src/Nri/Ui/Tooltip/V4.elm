module Nri.Ui.Tooltip.V4 exposing
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
    , onToggle
    , open
    , css, notMobileCss, mobileCss, quizEngineMobileCss, narrowMobileCss, containerCss
    , custom
    , nriDescription, testId
    , primaryLabel, auxiliaryDescription, disclosure
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
@docs onToggle
@docs open
@docs css, notMobileCss, mobileCss, quizEngineMobileCss, narrowMobileCss, containerCss
@docs custom
@docs nriDescription, testId
@docs primaryLabel, auxiliaryDescription, disclosure

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Content
import Css exposing (Color, Px, Style)
import Css.Global as Global
import Css.Media
import EventExtras as Events
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Maybe.Extra as Maybe
import Nri.Ui
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery exposing (mobileBreakpoint, narrowMobileBreakpoint, quizEngineBreakpoint)
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
            , width = Exactly 320
            , padding = NormalPadding
            , trigger = Nothing
            , triggerAttributes = []
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


isOffCenter : Alignment -> Bool
isOffCenter alignment =
    case alignment of
        Middle ->
            False

        _ ->
            True


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


type Orientation
    = Horizontal
    | Vertical


orientation : Direction -> Orientation
orientation direction =
    case direction of
        OnTop ->
            Vertical

        OnBottom ->
            Vertical

        OnLeft ->
            Horizontal

        OnRight ->
            Horizontal


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


{-| Set styles that will only apply if the viewport is narrower than NRI's narrow mobile breakpoint.

Equivalent to:

    Tooltip.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.narrowMobile ] styles ]

-}
narrowMobileCss : List Style -> Attribute msg
narrowMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.narrowMobile ] styles ]


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
                        [ Aria.expanded tooltip.isOpen
                        , Aria.controls [ id ]
                        ]
                 )
                    ++ buttonEvents
                    ++ tooltip.triggerAttributes
                    ++ [ Attributes.css
                            (case ( tooltip.tail, tooltip.isOpen ) of
                                ( WithTail, True ) ->
                                    [ tailForDirection tooltip.direction ]

                                _ ->
                                    []
                            )
                       ]
                )
            ]
        , viewTooltip id tooltip
        ]


viewTooltip : String -> Tooltip msg -> Html msg
viewTooltip tooltipId config =
    let
        mobileDirection =
            Maybe.withDefault config.direction config.mobileDirection

        quizEngineMobileDirection =
            Maybe.withDefault mobileDirection config.quizEngineMobileDirection

        narrowMobileDirection =
            Maybe.withDefault quizEngineMobileDirection config.narrowMobileDirection

        mobileAlignment =
            Maybe.withDefault config.alignment config.mobileAlignment

        quizEngineMobileAlignment =
            Maybe.withDefault mobileAlignment config.quizEngineMobileAlignment

        narrowMobileAlignment =
            Maybe.withDefault quizEngineMobileAlignment config.narrowMobileAlignment

        positionStyles =
            { mobile = positionTooltip mobileDirection mobileAlignment
            , quizEngineMobile = positionTooltip quizEngineMobileDirection quizEngineMobileAlignment
            , narrowMobile = positionTooltip narrowMobileDirection narrowMobileAlignment
            , default = positionTooltip config.direction config.alignment
            }
    in
    Html.div
        [ Attributes.css
            [ Css.position Css.absolute
            , MediaQuery.withViewport (Just mobileBreakpoint) Nothing positionStyles.default.container
            , MediaQuery.withViewport (Just quizEngineBreakpoint) (Just mobileBreakpoint) positionStyles.mobile.container
            , MediaQuery.withViewport (Just narrowMobileBreakpoint) (Just quizEngineBreakpoint) positionStyles.quizEngineMobile.container
            , MediaQuery.withViewport Nothing (Just narrowMobileBreakpoint) positionStyles.narrowMobile.container
            , Css.boxSizing Css.borderBox
            , if config.isOpen then
                Css.batch []

              else
                Css.display Css.none
            ]
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
                 , Css.border3 (Css.px 1) Css.solid outlineColor
                 , MediaQuery.withViewport (Just mobileBreakpoint) Nothing positionStyles.default.inner
                 , MediaQuery.withViewport (Just quizEngineBreakpoint) (Just mobileBreakpoint) positionStyles.mobile.inner
                 , MediaQuery.withViewport (Just narrowMobileBreakpoint) (Just quizEngineBreakpoint) positionStyles.quizEngineMobile.inner
                 , MediaQuery.withViewport Nothing (Just narrowMobileBreakpoint) positionStyles.narrowMobile.inner
                 , Fonts.baseFont
                 , Css.fontSize (Css.px 15)
                 , Css.fontWeight (Css.int 600)
                 , Css.color Colors.white
                 , Shadows.high
                 , Global.descendants
                    [ Global.a
                        [ Css.color Colors.white
                        , Css.borderColor Colors.white
                        , Css.visited [ Css.color Colors.white ]
                        , Css.hover [ Css.color Colors.white ]
                        , Css.pseudoClass "focus-visible"
                            [ Css.outline3 (Css.px 2) Css.solid Css.transparent
                            , Css.property "box-shadow" "0 0 0 2px #FFF"
                            ]
                        ]
                    ]
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
            (config.content
                ++ [ Html.div
                        [ ExtraAttributes.nriDescription "tooltip-hover-bridge"
                        , Attributes.css
                            [ Css.position Css.absolute
                            , MediaQuery.withViewport (Just mobileBreakpoint) Nothing <|
                                hoverAreaForDirection config.direction
                            , MediaQuery.withViewport (Just quizEngineBreakpoint) (Just mobileBreakpoint) <|
                                hoverAreaForDirection mobileDirection
                            , MediaQuery.withViewport (Just narrowMobileBreakpoint) (Just quizEngineBreakpoint) <|
                                hoverAreaForDirection quizEngineMobileDirection
                            , MediaQuery.withViewport Nothing (Just narrowMobileBreakpoint) <|
                                hoverAreaForDirection narrowMobileDirection
                            ]
                        ]
                        []
                   ]
            )
        ]


tailSize : Float
tailSize =
    8


tailGap : Float
tailGap =
    2


tooltipGap : Float
tooltipGap =
    tailSize + tailGap


tooltipColor : Color
tooltipColor =
    Colors.navy


outlineColor : Color
outlineColor =
    Css.rgba 255 255 255 0.5


offCenterOffset : Float
offCenterOffset =
    20



{-

     In the following functions:

     `mainAxis` refers to the axis on which `direction` affects the tooltip

     `crossAxis` refers to the axis on which `alignment` affects the tooltip

      Another way of thinking about this is:

      case orientation direction of
          Horizontal ->
              -- mainAxis is Y-axis (horizontal axis)
              -- crossAxis is X-axis

          Vertical ->
              -- vice versa

      Or for the visually minded:

        Main Axis
           |
           |
       ____|______
   ---|----|-----|------ Cross Axis
      |___ | ____|
          \|/
          _|__
         | | |
         ‾‾|‾‾
           |
-}


{-| This returns absolute positioning styles for the popout container for a given direction and alignment
-}
positionTooltip : Direction -> Alignment -> { container : List Style, inner : List Style }
positionTooltip direction alignment =
    let
        -- quick hack to make elm happy with different css units
        asCalc =
            Css.calc (Css.px 0) Css.plus

        positionMainAxis =
            case direction of
                OnTop ->
                    Css.top

                OnBottom ->
                    Css.bottom

                OnLeft ->
                    Css.left

                OnRight ->
                    Css.right

        ( translateMainAxis, translateCrossAxis ) =
            case orientation direction of
                Horizontal ->
                    ( Css.translateX, Css.translateY )

                Vertical ->
                    ( Css.translateY, Css.translateX )

        mainAxisTranslation =
            if direction == OnTop || direction == OnLeft then
                Just <| Css.pct -100

            else
                Nothing

        ( crossAxisPosition, crossAxisTranslation ) =
            (case alignment of
                Start customOffset ->
                    ( customOffset |> asCalc
                    , Css.pct 0
                    )

                Middle ->
                    ( Css.pct 50 |> asCalc
                    , Css.pct -50
                    )

                End customOffset ->
                    ( customOffset |> asCalc
                    , Css.pct -100
                    )
            )
                |> Tuple.mapFirst
                    (if isOffCenter alignment then
                        Css.calc (Css.px -offCenterOffset) Css.plus

                     else
                        identity
                    )

        positionCrossAxis =
            case ( orientation direction, alignment ) of
                ( Horizontal, End _ ) ->
                    Css.bottom

                ( Horizontal, _ ) ->
                    Css.top

                ( Vertical, End _ ) ->
                    Css.right

                ( Vertical, _ ) ->
                    Css.left
    in
    { container =
        [ positionMainAxis (Css.px (negate tooltipGap))
        , positionCrossAxis crossAxisPosition
        ]
    , inner =
        [ Css.transforms
            ([ translateCrossAxis crossAxisTranslation ]
                |> Maybe.cons (Maybe.map translateMainAxis mainAxisTranslation)
            )
        ]
    }


tailForDirection : Direction -> Style
tailForDirection direction =
    let
        applyTailColor =
            case direction of
                OnTop ->
                    Css.borderTopColor

                OnBottom ->
                    Css.borderBottomColor

                OnLeft ->
                    Css.borderLeftColor

                OnRight ->
                    Css.borderRightColor

        commonStyles =
            [ Css.borderStyle Css.solid
            , Css.borderColor Css.transparent
            , Css.property "content" "\" \""
            , Css.height Css.zero
            , Css.width Css.zero
            , Css.zIndex (Css.int 101)
            , Css.position Css.absolute
            , Css.pointerEvents Css.none
            , (case direction of
                OnTop ->
                    Css.top

                OnBottom ->
                    Css.bottom

                OnLeft ->
                    Css.left

                OnRight ->
                    Css.right
              )
                (Css.px (negate (tooltipGap + 1)))
            ]
    in
    Css.batch
        (([ Css.before, Css.after ]
            -- apply : a -> List (a -> b) -> List b
            |> (List.map << (|>)) commonStyles
         )
            ++ (case orientation direction of
                    Horizontal ->
                        [ Css.before
                            [ applyTailColor outlineColor
                            , Css.borderWidth (Css.px (tailSize + 1))
                            ]
                        , Css.after
                            [ applyTailColor tooltipColor
                            , Css.borderWidth (Css.px tailSize)
                            , Css.marginTop (Css.pct -50)
                            ]
                        ]

                    Vertical ->
                        [ Css.before
                            [ applyTailColor outlineColor
                            , Css.borderWidth (Css.px (tailSize + 1))
                            , Css.marginLeft (Css.px (-tailSize - 1))
                            ]
                        , Css.after
                            [ applyTailColor tooltipColor
                            , Css.borderWidth (Css.px tailSize)
                            , Css.marginLeft (Css.px -tailSize)
                            ]
                        ]
               )
        )


hoverAreaForDirection : Direction -> List Style
hoverAreaForDirection direction =
    let
        ( positionCrossAxisLower, positionCrossAxisUpper, mainAxisSize ) =
            case orientation direction of
                Horizontal ->
                    ( Css.bottom, Css.top, Css.width )

                Vertical ->
                    ( Css.left, Css.right, Css.height )

        positionMainAxis =
            case direction of
                OnTop ->
                    Css.top

                OnBottom ->
                    Css.bottom

                OnLeft ->
                    Css.left

                OnRight ->
                    Css.right
    in
    [ mainAxisSize (Css.px (tailSize + 3))
    , positionMainAxis (Css.pct 100)
    , positionCrossAxisLower Css.zero
    , positionCrossAxisUpper Css.zero
    ]
