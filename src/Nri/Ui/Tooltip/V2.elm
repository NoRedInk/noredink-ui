module Nri.Ui.Tooltip.V2 exposing
    ( view, toggleTip
    , Attribute
    , plaintext, html
    , onTop, onBottom, onLeft, onRight
    , alignStart, alignMiddle, alignEnd
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , onClick, onHover
    , open
    , css, containerCss
    , custom, customTriggerAttributes
    , nriDescription, testId
    , primaryLabel, auxillaryDescription
    )

{-| Known issues:

  - tooltips with focusable content (e.g., a link) will not handle focus correctly for
    keyboard-only users when using the onHover attribute

Post-release patches:

  - fix overlay for onClick toolTip having a border
  - mark customTriggerAttributes as deprecated
  - add containerCss
  - adds `nriDescription` and `testId`

Changes from V1:

  - {Position, withPosition} -> {onTop, onBottom, onLeft, onRight}
  - withTooltipStyleOverrides -> css
  - {Width, withWidth} -> {exactWidth, fitToContent}
  - {Padding, withPadding} -> {smallPadding, normalPadding}
  - adds customPadding
  - adds custom for custom attributes
  - adds plaintext, html helpers for setting the content
  - pass a list of attributes rather than requiring a pipeline to set up the tooltip
  - move Trigger into the attributes
  - change primaryLabel and auxillaryDescription to attributes, adding view
  - move the onTrigger event to the attributes
  - extraButtonAttrs becomes attribute `customTriggerAttributes`
  - isOpen field becomes the `open` attribute
  - fold toggleTip and view into each other, so there's less to maintain

These tooltips follow the accessibility recommendations from: <https://inclusive-components.design/tooltips-toggletips>

Example usage:

        Tooltip.view
            { trigger =
                \attrs ->
                    ClickableText.button "Click me to open the tooltip"
                        [ ClickableText.custom attrs ]
            , id = "my-tooltip"
            }
            [ Tooltip.plaintext "Gradebook"
            , Tooltip.primaryLabel
            , Tooltip.onClick MyOnTriggerMsg
            , Tooltip.open True
            ]

@docs view, toggleTip
@docs Attribute
@docs plaintext, html
@docs onTop, onBottom, onLeft, onRight
@docs alignStart, alignMiddle, alignEnd
@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs onClick, onHover
@docs open
@docs css, containerCss
@docs custom, customTriggerAttributes
@docs nriDescription, testId
@docs primaryLabel, auxillaryDescription

-}

import Accessibility.Styled as Html exposing (Attribute, Html, text)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css exposing (Color, Px, Style)
import Css.Global as Global
import EventExtras
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Ui
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import String.Extra


{-| -}
type Attribute msg
    = Attribute (Tooltip msg -> Tooltip msg)


type alias Tooltip msg =
    { direction : Direction
    , alignment : Alignment
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


{-| Where should the arrow be positioned relative to the tooltip?
-}
type Alignment
    = Start Px
    | Middle
    | End Px


withAligment : Alignment -> Attribute msg
withAligment alignment =
    Attribute (\config -> { config | alignment = alignment })


{-| Put the arrow at the "start" of the tooltip.
For onTop & onBottom tooltips, this means "left".
For onLeft & onRight tooltip, this means "top".

     __________
    |_  ______|
      \/

-}
alignStart : Px -> Attribute msg
alignStart position =
    withAligment (Start position)


{-| Put the arrow at the "middle" of the tooltip. This is the default behavior.

     __________
    |___  ____|
        \/

-}
alignMiddle : Attribute msg
alignMiddle =
    withAligment Middle


{-| Put the arrow at the "end" of the tooltip.
For onTop & onBottom tooltips, this means "right".
For onLeft & onRight tooltip, this means "bottom".

     __________
    |______  _|
           \/

-}
alignEnd : Px -> Attribute msg
alignEnd position =
    withAligment (End position)


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


{-| Set some custom styles on the tooltip. These will be treated as overrides,
so be careful!
-}
css : List Style -> Attribute msg
css tooltipStyleOverrides =
    Attribute (\config -> { config | tooltipStyleOverrides = tooltipStyleOverrides })


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


{-| DEPRECATED -- a future release will remove this helper.
-}
customTriggerAttributes : List (Html.Attribute msg) -> Attribute msg
customTriggerAttributes attributes =
    Attribute (\config -> { config | triggerAttributes = config.triggerAttributes ++ attributes })


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
    | OnClick (Bool -> msg)


{-| The tooltip opens when hovering over the trigger element, and closes when the hover stops.
-}
onHover : (Bool -> msg) -> Attribute msg
onHover msg =
    Attribute (\config -> { config | trigger = Just (OnHover msg) })


{-| The tooltip opens when clicking the root element, and closes when anything but the tooltip is clicked again.
-}
onClick : (Bool -> msg) -> Attribute msg
onClick msg =
    Attribute (\config -> { config | trigger = Just (OnClick msg) })


type Purpose
    = PrimaryLabel
    | AuxillaryDescription


{-| Used when the content of the tooltip is the "primary label" for its content, for example,
when the trigger content is an icon. The tooltip content will supercede the content of the trigger
HTML for screen readers.

This is the default.

-}
primaryLabel : Attribute msg
primaryLabel =
    Attribute (\config -> { config | purpose = PrimaryLabel })


{-| Used when the content of the tooltip provides an "auxillary description" for its content.
-}
auxillaryDescription : Attribute msg
auxillaryDescription =
    Attribute (\config -> { config | purpose = AuxillaryDescription })


{-| -}
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
toggleTip : { label : String } -> List (Attribute msg) -> Html msg
toggleTip { label } attributes_ =
    let
        id =
            String.Extra.dasherize label
    in
    view
        { trigger =
            \events ->
                ClickableSvg.button label
                    UiIcon.help
                    [ ClickableSvg.exactWidth 20
                    , ClickableSvg.exactHeight 20
                    , ClickableSvg.custom events
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
                Just (OnClick msg) ->
                    ( []
                    , [ EventExtras.onClickStopPropagation
                            (msg (not tooltip.isOpen))
                      ]
                    )

                Just (OnHover msg) ->
                    ( [ Events.onMouseEnter (msg True)
                      , Events.onMouseLeave (msg False)
                      ]
                    , [ Events.onFocus (msg True)

                      -- TODO: this blur event means that we cannot focus links
                      -- that are within the tooltip without a mouse
                      , Events.onBlur (msg False)
                      , Events.onClick (msg True)
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
            []
            [ trigger
                ([ if tooltip.isOpen then
                    case tooltip.purpose of
                        PrimaryLabel ->
                            Aria.labeledBy id

                        AuxillaryDescription ->
                            Aria.describedBy [ id ]

                   else
                    -- when our tooltips are closed, they're not rendered in the
                    -- DOM. This means that the ID references above would be
                    -- invalid and jumping to a reference would not work, so we
                    -- skip labels and descriptions if the tooltip is closed.
                    Attributes.property "data-closed-tooltip" Encode.null
                 ]
                    ++ buttonEvents
                    ++ tooltip.triggerAttributes
                )
            , hoverBridge tooltip
            ]
        , viewOverlay tooltip

        -- Popout is rendered after the overlay, to allow client code to give it
        -- priority when clicking by setting its position
        , viewTooltip id tooltip
        ]


{-| This is a "bridge" for the cursor to move from trigger content to tooltip, so the user can click on links, etc.
-}
hoverBridge : Tooltip msg -> Html msg
hoverBridge { isOpen, direction, alignment } =
    let
        bridgeLength =
            arrowSize + 5
    in
    if isOpen then
        Nri.Ui.styled Html.div
            "tooltip-hover-bridge"
            [ Css.boxSizing Css.borderBox
            , Css.padding (Css.px arrowSize)
            , Css.position Css.absolute
            , Css.batch <|
                case direction of
                    OnTop ->
                        [ Css.top (Css.px -bridgeLength)
                        , Css.left Css.zero
                        , Css.width (Css.pct 100)
                        , Css.height (Css.px arrowSize)
                        ]

                    OnRight ->
                        [ Css.right (Css.px -bridgeLength)
                        , Css.top Css.zero
                        , Css.width (Css.px arrowSize)
                        , Css.height (Css.pct 100)
                        ]

                    OnBottom ->
                        [ Css.bottom (Css.px -bridgeLength)
                        , Css.left Css.zero
                        , Css.width (Css.pct 100)
                        , Css.height (Css.px arrowSize)
                        ]

                    OnLeft ->
                        [ Css.left (Css.px -bridgeLength)
                        , Css.top Css.zero
                        , Css.width (Css.px arrowSize)
                        , Css.height (Css.pct 100)
                        ]
            ]
            []
            []

    else
        text ""


viewTooltip : String -> Tooltip msg -> Html msg
viewTooltip tooltipId config =
    if config.isOpen then
        viewOpenTooltip tooltipId config

    else
        text ""


viewOpenTooltip : String -> Tooltip msg -> Html msg
viewOpenTooltip tooltipId config =
    Html.div
        [ Attributes.css
            [ Css.position Css.absolute
            , positionTooltip config.direction config.alignment
            , Css.boxSizing Css.borderBox
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
                 ]
                    ++ config.tooltipStyleOverrides
                )
             , pointerBox config.direction config.alignment

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


arrowSize : Float
arrowSize =
    8


tooltipColor : Color
tooltipColor =
    Colors.navy


offCenterOffset : Float
offCenterOffset =
    20


{-| This returns an absolute positioning style attribute for the popout container for a given arrow position.
-}
positionTooltip : Direction -> Alignment -> Style
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
    Css.batch <|
        case direction of
            OnTop ->
                [ ltrPosition
                , Css.top (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
                ]

            OnBottom ->
                [ ltrPosition
                , Css.bottom (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
                ]

            OnLeft ->
                [ topToBottomPosition
                , Css.left (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
                ]

            OnRight ->
                [ topToBottomPosition
                , Css.right (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
                ]


pointerBox : Direction -> Alignment -> Html.Attribute msg
pointerBox direction alignment =
    Attributes.css
        [ Css.backgroundColor Colors.navy
        , Css.border3 (Css.px 1) Css.solid Colors.navy
        , arrowInPosition direction alignment
        , Fonts.baseFont
        , Css.fontSize (Css.px 16)
        , Css.fontWeight (Css.int 600)
        , Css.color Colors.white
        , Global.descendants [ Global.a [ Css.textDecoration Css.underline ] ]
        , Global.descendants [ Global.a [ Css.color Colors.white ] ]
        ]


viewOverlay : Tooltip msg -> Html msg
viewOverlay { isOpen, trigger } =
    case ( isOpen, trigger ) of
        ( True, Just (OnClick msg) ) ->
            -- if we display the click-to-close overlay on hover, you will have to
            -- close the overlay by moving the mouse out of the window or clicking.
            viewCloseTooltipOverlay (msg False)

        _ ->
            text ""


viewCloseTooltipOverlay : msg -> Html msg
viewCloseTooltipOverlay msg =
    Html.button
        [ Attributes.css
            [ Css.width (Css.pct 100)
            , -- ancestor uses transform property, which interacts with
              -- position: fixed, forcing this hack.
              -- https://www.w3.org/TR/css-transforms-1/#propdef-transform
              Css.height (Css.calc (Css.px 1000) Css.plus (Css.calc (Css.pct 100) Css.plus (Css.px 1000)))
            , Css.left Css.zero
            , Css.top (Css.px -1000)
            , Css.cursor Css.pointer
            , Css.position Css.fixed
            , Css.zIndex (Css.int 90) -- TODO: From Nri.ZIndex in monolith, bring ZIndex here?
            , Css.backgroundColor Css.transparent
            , Css.border Css.zero
            , Css.outline Css.none
            ]
        , EventExtras.onClickStopPropagation msg
        , Key.tabbable False
        ]
        []


tooltipContainerStyles : List Style
tooltipContainerStyles =
    [ Css.display Css.inlineBlock
    , Css.textAlign Css.left
    , Css.position Css.relative
    ]



-- ARROWS


arrowInPosition : Direction -> Alignment -> Style
arrowInPosition position alignment =
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
                    Css.property "top" ("calc(-" ++ String.fromFloat arrowSize ++ "px + " ++ String.fromFloat offCenterOffset ++ "px)")

                Middle ->
                    Css.property "top" ("calc(-" ++ String.fromFloat arrowSize ++ "px + 50%)")

                End _ ->
                    Css.property "bottom" ("calc(-" ++ String.fromFloat arrowSize ++ "px + " ++ String.fromFloat offCenterOffset ++ "px)")
    in
    case position of
        OnTop ->
            Css.batch
                [ Css.property "transform" "translate(-50%, -100%)"
                , getArrowPositioning
                    { xAlignment = topBottomAlignment
                    , yAlignment = Css.top (Css.pct 100)
                    }
                , bottomArrow
                ]

        OnBottom ->
            Css.batch
                [ Css.property "transform" "translate(-50%, 0)"
                , getArrowPositioning
                    { xAlignment = topBottomAlignment
                    , yAlignment = Css.bottom (Css.pct 100)
                    }
                , topArrow
                ]

        OnRight ->
            Css.batch
                [ Css.property "transform" "translate(0, -50%)"
                , getArrowPositioning
                    { xAlignment = Css.right (Css.pct 100)
                    , yAlignment = rightLeftAlignment
                    }
                , leftArrow
                ]

        OnLeft ->
            Css.batch
                [ Css.property "transform" "translate(-100%, -50%)"
                , getArrowPositioning
                    { xAlignment = Css.left (Css.pct 100)
                    , yAlignment = rightLeftAlignment
                    }
                , rightArrow
                ]


bottomArrow : Style
bottomArrow =
    Css.batch
        [ Css.before
            [ Css.borderTopColor tooltipColor
            , Css.property "border-width" (String.fromFloat (arrowSize + 1) ++ "px")
            , Css.marginLeft (Css.px (-arrowSize - 1))
            ]
        , Css.after
            [ Css.borderTopColor tooltipColor
            , Css.property "border-width" (String.fromFloat arrowSize ++ "px")
            , Css.marginLeft (Css.px -arrowSize)
            ]
        ]


topArrow : Style
topArrow =
    Css.batch
        [ Css.before
            [ Css.borderBottomColor tooltipColor
            , Css.property "border-width" (String.fromFloat (arrowSize + 1) ++ "px")
            , Css.marginLeft (Css.px (-arrowSize - 1))
            ]
        , Css.after
            [ Css.borderBottomColor tooltipColor
            , Css.property "border-width" (String.fromFloat arrowSize ++ "px")
            , Css.marginLeft (Css.px -arrowSize)
            ]
        ]


rightArrow : Style
rightArrow =
    Css.batch
        [ Css.before
            [ Css.borderLeftColor tooltipColor
            , Css.property "border-width" (String.fromFloat (arrowSize + 1) ++ "px")
            ]
        , Css.after
            [ Css.borderLeftColor tooltipColor
            , Css.property "border-width" (String.fromFloat arrowSize ++ "px")
            , Css.marginTop (Css.px 1)
            , Css.marginRight (Css.px 2)
            ]
        ]


leftArrow : Style
leftArrow =
    Css.batch
        [ Css.before
            [ Css.borderRightColor tooltipColor
            , Css.property "border-width" (String.fromFloat (arrowSize + 1) ++ "px")
            ]
        , Css.after
            [ Css.borderRightColor tooltipColor
            , Css.property "border-width" (String.fromFloat arrowSize ++ "px")
            , Css.marginTop (Css.px 1)
            , Css.marginLeft (Css.px 2)
            ]
        ]


getArrowPositioning : { xAlignment : Style, yAlignment : Style } -> Style
getArrowPositioning config =
    Css.batch
        [ Css.before (positionArrow config)
        , Css.after (positionArrow config)
        ]


positionArrow : { xAlignment : Style, yAlignment : Style } -> List Style
positionArrow { xAlignment, yAlignment } =
    [ xAlignment
    , yAlignment
    , Css.property "border" "solid transparent"
    , Css.property "content" "\" \""
    , Css.height Css.zero
    , Css.width Css.zero
    , Css.position Css.absolute
    , Css.pointerEvents Css.none
    ]
