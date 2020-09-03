module Nri.Ui.Tooltip.V2 exposing
    ( view, toggleTip
    , plaintext, html
    , onTop, onBottom, onLeft, onRight
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , onClick, onHover
    , css, custom
    , primaryLabel, auxillaryDescription
    )

{-| Changes from V1:

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

These tooltips follow the accessibility recommendations from: <https://inclusive-components.design/tooltips-toggletips>

Example usage:

        Tooltip.view
            { triggerHtml = someTriggerHtml
            , onTrigger = MyOnTriggerMsg
            , isOpen = True
            , extraButtonAttrs = modalV9LastFocusableElement
            }
            [ Tooltip.plaintext "Gradebook"
            , Tooltip.primaryLabel
            , Tooltip.smallPadding
            , Tooltip.fitToContent
            , Tooltip.onClick
            ]


## Suggested Improvements for V2

  - The toggle tip does not currently manage focus correctly for keyboard users - if a
    user tries to click on a link in the toggle tip, the tip will disappear as focus moves
    to the next item in the page. This should be improved in the next release.
  - Currently, only toggle tip supports links on hover - generalize this to all tooltips

@docs view, toggleTip

@docs plaintext, html
@docs onTop, onBottom, onLeft, onRight
@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs onClick, onHover
@docs css, custom
@docs primaryLabel, auxillaryDescription

-}

import Accessibility.Styled as Html exposing (Attribute, Html, text)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (Color, Style)
import Css.Global as Global
import EventExtras
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Svg.Styled as Svg exposing (Svg, circle, g, svg)
import Svg.Styled.Attributes exposing (cx, cy, d, fill, fillRule, height, r, stroke, strokeWidth, viewBox, width)


{-| -}
type Attribute msg
    = Attribute (Tooltip msg -> Tooltip msg)


type alias Tooltip msg =
    { position : Direction
    , content : List (Html msg)
    , attributes : List (Html.Attribute Never)
    , tooltipStyleOverrides : List Style
    , width : Width
    , padding : Padding
    , trigger : Trigger
    , purpose : Purpose
    }


buildAttributes : List (Attribute msg) -> Tooltip msg
buildAttributes =
    let
        defaultTooltip : Tooltip msg
        defaultTooltip =
            { position = OnTop
            , content = []
            , attributes = []
            , tooltipStyleOverrides = []
            , width = Exactly 320
            , padding = NormalPadding
            , trigger = OnHover
            , purpose = PrimaryLabel
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


{-| Where should this tooltip be positioned?
-}
type Direction
    = OnTop
    | OnBottom
    | OnLeft
    | OnRight


withPosition : Direction -> Attribute msg
withPosition position =
    Attribute (\config -> { config | position = position })


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
    Attribute (\config -> { config | attributes = attributes })


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


type Trigger
    = OnHover
    | OnClick


{-| The tooltip opens when hovering over the trigger element, and closes when the hover stops.

Note: design typically prefers `OnHover`. However, if your tooltip has a link that someone
needs to click, use `OnClick` because hover tooltips will currently close when you try
to click the link.

FIXME: Make it so you can click on links in hover tooltips.

This is the default.

-}
onHover : Attribute msg
onHover =
    Attribute (\config -> { config | trigger = OnHover })


{-| The tooltip opens when clicking the root element, and closes when anything but the tooltip is clicked again.
-}
onClick : Attribute msg
onClick =
    Attribute (\config -> { config | trigger = OnHover })


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


{-| Here's what the fields in the configuration record do:

  - `triggerHtml`: What element do you interact with to open the tooltip?
  - `extraButtonAttrs`: Adds attributes to the trigger button. Useful for things like focus management, like with Accessible Modal
  - `onTrigger`: What `msg` should I send when the tooltip should open and
    close? The `Bool` represents the next `isOpen` value.
  - `isOpen`: Is the tooltip open now? (keep track of this in your model somewhere)
  - `id`: A unique identifier used to associate the trigger with its content

-}
view :
    { triggerHtml : Html msg
    , extraButtonAttrs : List (Html.Attribute msg)
    , onTrigger : Bool -> msg
    , isOpen : Bool
    , id : String
    }
    -> List (Attribute msg)
    -> Html msg
view config attributes =
    viewTooltip_ config (buildAttributes attributes)


{-| Supplementary information triggered by a "?" icon

A toggle tip is always triggered by a hover (or focus, for keyboard users)

-}
toggleTip :
    { onTrigger : Bool -> msg
    , isOpen : Bool
    , label : String
    , extraButtonAttrs : List (Html.Attribute msg)
    }
    -> List (Attribute msg)
    -> Html msg
toggleTip { isOpen, onTrigger, extraButtonAttrs, label } attributes_ =
    let
        contentSize =
            20

        attributes =
            buildAttributes attributes_
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-Tooltip-V2-ToggleTip"
        (tooltipContainerStyles
            ++ [ -- Take up enough room within the document flow
                 Css.width (Css.px contentSize)
               , Css.height (Css.px contentSize)
               , Css.margin (Css.px 5)
               ]
        )
        []
        [ Html.button
            ([ Widget.label label
             , Attributes.css buttonStyleOverrides
             ]
                ++ eventsForTrigger attributes.trigger onTrigger
                ++ extraButtonAttrs
            )
            [ hoverBridge contentSize
                [ Html.div
                    [ Attributes.css
                        [ Css.position Css.relative
                        , Css.width (Css.px contentSize)
                        , Css.height (Css.px contentSize)
                        , Css.color Colors.azure
                        ]
                    ]
                    [ iconHelp
                    , Html.span
                        [ -- This adds aria-live polite & also aria-live atomic, so our screen readers are alerted when content appears
                          Role.status
                        ]
                        [ viewIf (\_ -> viewTooltip Nothing attributes) isOpen ]
                    ]
                ]
            ]
        ]


{-| Provides a "bridge" for the cursor to move from trigger content to tooltip, so the user can click on links, etc.

Works by being larger than the trigger content & overlaying it, but is removed from the flow of the page (position: absolute), so that it looks ok visually.

-}
hoverBridge : Float -> List (Html msg) -> Html msg
hoverBridge contentSize =
    let
        padding =
            -- enough to cover the empty gap between tooltip and trigger content
            10
    in
    Nri.Ui.styled Html.div
        "tooltip-hover-bridge"
        [ Css.boxSizing Css.borderBox
        , Css.padding (Css.px padding)
        , Css.width (Css.px <| contentSize + padding * 2)
        , Css.height (Css.px <| contentSize + padding * 2)
        , Css.position Css.absolute
        , Css.top (Css.px <| negate padding)
        , Css.left (Css.px <| negate padding)
        ]
        []


{-| Made with <https://levelteams.com/svg-to-elm>
-}
iconHelp : Svg msg
iconHelp =
    svg [ width "20px", height "20px", viewBox "0 0 25 25" ]
        [ g [ stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ]
            [ circle [ stroke "#146AFF", strokeWidth "2", cx "12.5", cy "12.5", r "11.5" ] []
            , Svg.path [ d "M12.6825,6.6275 C13.3866702,6.6275 14.0095806,6.74395717 14.55125,6.976875 C15.0929194,7.20979283 15.5154151,7.53749789 15.81875,7.96 C16.1220849,8.38250211 16.27375,8.86458063 16.27375,9.40625 C16.27375,9.98041954 16.1329181,10.470623 15.85125,10.876875 C15.5695819,11.283127 15.1579194,11.7408308 14.61625,12.25 C14.1937479,12.6508353 13.8768761,12.9866653 13.665625,13.2575 C13.4543739,13.5283347 13.3216669,13.8262484 13.2675,14.15125 L13.18625,14.6875 L11.74,14.6875 L11.74,13.875 C11.74,13.3116639 11.8402073,12.8458352 12.040625,12.4775 C12.2410427,12.1091648 12.5362481,11.6975023 12.92625,11.2425 C13.2079181,10.9174984 13.419166,10.6385428 13.56,10.405625 C13.700834,10.1727072 13.77125,9.91541808 13.77125,9.63375 C13.77125,9.30874838 13.6602094,9.0595842 13.438125,8.88625 C13.2160406,8.7129158 12.8991687,8.62625 12.4875,8.62625 C11.7074961,8.62625 10.9437537,8.85916434 10.19625,9.325 L10.19625,7.29375 C10.9112536,6.84958111 11.7399953,6.6275 12.6825,6.6275 Z M11.17125,18.34375 L11.17125,15.7275 L13.82,15.7275 L13.82,18.34375 L11.17125,18.34375 Z", fill "#146AFF" ]
                []
            ]
        ]



-- INTERNALS


viewTooltip_ :
    { triggerHtml : Html msg
    , onTrigger : Bool -> msg
    , isOpen : Bool
    , id : String -- Accessibility: Used to match tooltip to trigger
    , extraButtonAttrs : List (Html.Attribute msg)
    }
    -> Tooltip msg
    -> Html msg
viewTooltip_ { triggerHtml, onTrigger, isOpen, id, extraButtonAttrs } tooltip_ =
    Nri.Ui.styled Html.div
        "Nri-Ui-Tooltip-V2"
        tooltipContainerStyles
        []
        [ Html.button
            ([ if isOpen then
                case tooltip_.purpose of
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
             , Attributes.css buttonStyleOverrides
             ]
                ++ eventsForTrigger tooltip_.trigger onTrigger
                ++ extraButtonAttrs
            )
            [ triggerHtml ]

        -- if we display the click-to-close overlay on hover, you will have to
        -- close the overlay by moving the mouse out of the window or clicking.
        , viewIf (\_ -> viewCloseTooltipOverlay (onTrigger False))
            (isOpen && tooltip_.trigger == OnClick)

        -- Popout is rendered after the overlay, to allow client code to give it
        -- priority when clicking by setting its position
        , viewIf (\_ -> viewTooltip (Just id) tooltip_) isOpen
        ]


{-| TODO: Move this somewhere if it becomes useful in other modules here
-}
viewIf : (() -> Html msg) -> Bool -> Html msg
viewIf viewFn condition =
    case condition of
        True ->
            viewFn ()

        False ->
            Html.text ""


viewTooltip : Maybe String -> Tooltip msg -> Html msg
viewTooltip maybeTooltipId config =
    Html.div [ Attributes.css (containerPositioningForArrowPosition config.position) ]
        [ Html.div
            ([ Attributes.css
                ([ Css.borderRadius (Css.px 8)
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
             , pointerBox config.position

             -- We need to keep this animation in tests to make it pass: check out
             -- the NoAnimations middleware. So if you change the name here, please
             -- change that as well
             , Attributes.class "dont-disable-animation"
             , Role.toolTip
             ]
                ++ config.attributes
                ++ (case maybeTooltipId of
                        Just tooltipId ->
                            [ Attributes.id tooltipId ]

                        Nothing ->
                            []
                   )
            )
            config.content
        ]


eventsForTrigger : Trigger -> (Bool -> msg) -> List (Html.Attribute msg)
eventsForTrigger trigger msg =
    case trigger of
        OnClick ->
            [ EventExtras.onClickStopPropagation (msg True)
            , Events.onFocus (msg True)
            , Events.onBlur (msg False)
            ]

        OnHover ->
            [ Events.onMouseEnter (msg True)
            , Events.onMouseLeave (msg False)
            , Events.onFocus (msg True)
            , Events.onBlur (msg False)
            , EventExtras.onClickStopPropagation (msg True)
            ]


arrowSize : Float
arrowSize =
    8


tooltipColor : Color
tooltipColor =
    Colors.navy


{-| This returns an absolute positioning style attribute for the popout container for a given arrow position.
-}
containerPositioningForArrowPosition : Direction -> List Style
containerPositioningForArrowPosition arrowPosition =
    case arrowPosition of
        OnTop ->
            [ Css.left (Css.pct 50)
            , Css.top (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
            , Css.position Css.absolute
            ]

        OnBottom ->
            [ Css.left (Css.pct 50)
            , Css.bottom (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
            , Css.position Css.absolute
            ]

        OnLeft ->
            [ Css.top (Css.pct 50)
            , Css.left (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
            , Css.position Css.absolute
            ]

        OnRight ->
            [ Css.top (Css.pct 50)
            , Css.right (Css.calc (Css.px (negate arrowSize)) Css.minus (Css.px 2))
            , Css.position Css.absolute
            ]


pointerBox : Direction -> Html.Attribute msg
pointerBox position =
    Attributes.css
        [ Css.backgroundColor Colors.navy
        , Css.border3 (Css.px 1) Css.solid Colors.navy
        , arrowInPosition position
        , Fonts.baseFont
        , Css.fontSize (Css.px 16)
        , Css.fontWeight (Css.int 600)
        , Css.color Colors.white
        , Global.descendants [ Global.a [ Css.textDecoration Css.underline ] ]
        , Global.descendants [ Global.a [ Css.color Colors.white ] ]
        ]


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
            ]
        , EventExtras.onClickStopPropagation msg
        ]
        []


tooltipContainerStyles : List Style
tooltipContainerStyles =
    [ Css.display Css.inlineBlock
    , Css.textAlign Css.left
    , Css.position Css.relative
    ]


buttonStyleOverrides : List Style
buttonStyleOverrides =
    [ Css.cursor Css.pointer
    , Css.border Css.zero
    , Css.backgroundColor Css.transparent
    , Css.fontSize Css.inherit
    , Css.fontFamily Css.inherit
    , Css.color Css.inherit
    , Css.margin Css.zero
    , Css.padding Css.zero
    , Css.textAlign Css.left
    ]



-- ARROWS


arrowInPosition : Direction -> Style
arrowInPosition position =
    case position of
        OnTop ->
            newPositionVerticalAlignTooltip "-100%" Css.top bottomArrow

        OnBottom ->
            newPositionVerticalAlignTooltip "0" Css.bottom topArrow

        OnRight ->
            newPositionHorizontalAlignTooltip "0" Css.right leftArrow

        OnLeft ->
            newPositionHorizontalAlignTooltip "-100%" Css.left rightArrow


newPositionVerticalAlignTooltip : String -> (Css.Pct -> Style) -> Style -> Style
newPositionVerticalAlignTooltip verticalAlign arrowAlignment arrow =
    Css.batch
        [ Css.property "transform" ("translate(-50%, " ++ verticalAlign ++ ")")
        , getArrowPositioning
            { xAlignment = Css.left (Css.pct 50)
            , yAlignment = arrowAlignment (Css.pct 100)
            }
        , arrow
        ]


newPositionHorizontalAlignTooltip : String -> (Css.Pct -> Style) -> Style -> Style
newPositionHorizontalAlignTooltip horizontalAlign arrowAlignment arrow =
    Css.batch
        [ Css.property "transform" ("translate(" ++ horizontalAlign ++ ", -50%)")
        , getArrowPositioning
            { xAlignment = arrowAlignment (Css.pct 100)
            , yAlignment = Css.property "top" ("calc(-" ++ String.fromFloat arrowSize ++ "px + 50%)")
            }
        , arrow
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
