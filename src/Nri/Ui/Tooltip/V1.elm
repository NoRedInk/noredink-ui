module Nri.Ui.Tooltip.V1 exposing
    ( Tooltip, tooltip
    , Position(..), withPosition
    , Width(..), withWidth
    , Padding(..), withPadding
    , withTooltipStyleOverrides
    , Trigger(..)
    , primaryLabel, auxillaryDescription, toggleTip
    )

{-| A tooltip component!

These tooltips follow the accessibility recommendations from: <https://inclusive-components.design/tooltips-toggletips>

Example usage:

        tooltip [ Html.text "Gradebook" ]
        |> withPadding SmallPadding
        |> withWidth FitToContent
        |> primaryLabel {
            trigger = OnClick
            , triggerHtml = someTriggerHtml
            , onTrigger = MyOnTriggerMsg
            , isOpen = True
            , extraButtonAttrs = modalV9LastFocusableElement
        }


## Suggested Improvements for V2

  - The toggle tip does not currently manage focus correctly for keyboard users - if a
    user tries to click on a link in the toggle tip, the tip will disappear as focus moves
    to the next item in the page. This should be improved in the next release.
  - Currently, only toggle tip supports links on hover - generalize this to all tooltips


## Tooltip Construction

@docs Tooltip, tooltip

@docs Position, withPosition

@docs Width, withWidth

@docs Padding, withPadding

@docs withTooltipStyleOverrides

@docs Trigger


## View Functions

@docs primaryLabel, auxillaryDescription, toggleTip

-}

import Accessibility.Styled as Html exposing (Attribute, Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (Color, Style)
import Css.Global as Global
import EventExtras
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Svg.Styled as Svg exposing (Svg, circle, g, svg)
import Svg.Styled.Attributes exposing (cx, cy, d, fill, fillRule, height, r, stroke, strokeWidth, viewBox, width)


{-| A standard NoRedInk tooltip, which appears around its parent element.

Create versions of me with `tooltip`, and customize them with functions like
`withPosition` and `withTheme` from this model. Like so:

    tooltip [ Html.text "Hello, World!" ]
        |> withPosition OnLeft

-}
type Tooltip msg
    = Tooltip
        { position : Position
        , content : List (Html msg)

        -- extra styles
        , tooltipStyleOverrides : List Style
        , width : Width
        , padding : Padding
        }


{-| Construct a tooltip given some content.
-}
tooltip : List (Html msg) -> Tooltip msg
tooltip content =
    Tooltip
        { position = OnTop
        , content = content

        -- extra styles
        , tooltipStyleOverrides = []
        , width = Exactly 320
        , padding = NormalPadding
        }


{-| Where should this tooltip be positioned?
-}
type Position
    = OnTop
    | OnBottom
    | OnLeft
    | OnRight


{-| Set a tooltip's position:

    tooltip [ text "I'm on the left!" ]
        |> withPosition OnLeft

-}
withPosition : Position -> Tooltip msg -> Tooltip msg
withPosition position (Tooltip config) =
    Tooltip { config | position = position }


{-| Set some custom styles on the tooltip. These will be treated as overrides,
so be careful!
-}
withTooltipStyleOverrides : List Style -> Tooltip msg -> Tooltip msg
withTooltipStyleOverrides tooltipStyleOverrides (Tooltip config) =
    Tooltip { config | tooltipStyleOverrides = tooltipStyleOverrides }


{-| Should the tooltip be exactly some measurement or fit to the width of the
content?
-}
type Width
    = Exactly Int
    | FitToContent


{-| Set the width of the tooltip itself.
-}
withWidth : Width -> Tooltip msg -> Tooltip msg
withWidth width (Tooltip config) =
    Tooltip { config | width = width }


{-| How much padding should be around the content inside the tooltip?
-}
type Padding
    = SmallPadding
    | NormalPadding


{-| Set the padding around the edges of the tooltip.
-}
withPadding : Padding -> Tooltip msg -> Tooltip msg
withPadding padding (Tooltip config) =
    Tooltip { config | padding = padding }


{-| How do you open this tooltip?

  - `OnHover`: the tooltip opens when hovering over the trigger element, and
    closes when the hover stops.
  - `OnClick`: the tooltip opens when clicking the root element, and closes when
    anything but the tooltip is clicked again.

Note: design typically prefers `OnHover`. However, if your tooltip has a link that someone
needs to click, use `OnClick` because hover tooltips will currently close when you try
to click the link.

FIXME: Make it so you can click on links in hover tooltips.

-}
type Trigger
    = OnHover
    | OnClick


{-| Used when the content of the tooltip is the "primary label" for its content, for example,
when the trigger content is an icon. The tooltip content will supercede the content of the trigger
HTML for screen readers.

Here's what the fields in the configuration record do:

  - `trigger`: How do you open this tooltip?
  - `triggerHtml`: What element do you interact with to open the tooltip?
  - `extraButtonAttrs`: Adds attributes to the trigger button. Useful for things like focus management, like with Accessible Modal
  - `onTrigger`: What `msg` should I send when the tooltip should open and
    close? The `Bool` represents the next `isOpen` value.
  - `isOpen`: Is the tooltip open now? (keep track of this in your model somewhere)
  - `id`: A unique identifier used to associate the trigger with its content

-}
primaryLabel :
    { trigger : Trigger
    , triggerHtml : Html msg
    , extraButtonAttrs : List (Attribute msg)
    , onTrigger : Bool -> msg
    , isOpen : Bool
    , id : String
    }
    -> Tooltip msg
    -> Html msg
primaryLabel =
    viewTooltip_ PrimaryLabel


{-| Used when the content of the tooltip provides an "auxillary description" for its content.
-}
auxillaryDescription :
    { trigger : Trigger
    , triggerHtml : Html msg
    , extraButtonAttrs : List (Attribute msg)
    , onTrigger : Bool -> msg
    , isOpen : Bool
    , id : String
    }
    -> Tooltip msg
    -> Html msg
auxillaryDescription =
    viewTooltip_ AuxillaryDescription


{-| Supplementary information triggered by a "?" icon

A toggle tip is always triggered by a hover (or focus, for keyboard users)

-}
toggleTip :
    { onTrigger : Bool -> msg
    , isOpen : Bool
    , label : String
    , extraButtonAttrs : List (Attribute msg)
    }
    -> Tooltip msg
    -> Html msg
toggleTip { isOpen, onTrigger, extraButtonAttrs, label } tooltip_ =
    let
        contentSize =
            20
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-Tooltip-V1-ToggleTip"
        (tooltipContainerStyles
            ++ [ -- Take up enough room within the document flow
                 Css.width (Css.px contentSize)
               , Css.height (Css.px contentSize)
               , Css.margin (Css.px 5)
               ]
        )
        []
        [ Html.button
            ([ Aria.label label
             , css buttonStyleOverrides
             ]
                ++ eventsForTrigger OnHover onTrigger
                ++ extraButtonAttrs
            )
            [ hoverBridge contentSize
                [ Html.div
                    [ css
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
                        [ viewIf (\_ -> viewTooltip Nothing OnHover tooltip_) isOpen ]
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


type Purpose
    = PrimaryLabel
    | AuxillaryDescription


viewTooltip_ :
    Purpose
    ->
        { trigger : Trigger
        , triggerHtml : Html msg
        , onTrigger : Bool -> msg
        , isOpen : Bool
        , id : String -- Accessibility: Used to match tooltip to trigger
        , extraButtonAttrs : List (Attribute msg)
        }
    -> Tooltip msg
    -> Html msg
viewTooltip_ purpose { trigger, triggerHtml, onTrigger, isOpen, id, extraButtonAttrs } tooltip_ =
    Nri.Ui.styled Html.div
        "Nri-Ui-Tooltip-V1"
        tooltipContainerStyles
        []
        [ Html.button
            ([ if isOpen then
                case purpose of
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
             , css buttonStyleOverrides
             ]
                ++ eventsForTrigger trigger onTrigger
                ++ extraButtonAttrs
            )
            [ triggerHtml ]

        -- if we display the click-to-close overlay on hover, you will have to
        -- close the overlay by moving the mouse out of the window or clicking.
        , viewIf (\_ -> viewCloseTooltipOverlay (onTrigger False)) (isOpen && trigger == OnClick)

        -- Popout is rendered after the overlay, to allow client code to give it
        -- priority when clicking by setting its position
        , viewIf (\_ -> viewTooltip (Just id) trigger tooltip_) isOpen
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


viewTooltip : Maybe String -> Trigger -> Tooltip msg -> Html msg
viewTooltip maybeTooltipId trigger (Tooltip config) =
    Html.div [ css (containerPositioningForArrowPosition config.position) ]
        [ Html.div
            ([ css
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
                ++ (case maybeTooltipId of
                        Just tooltipId ->
                            [ Attributes.id tooltipId ]

                        Nothing ->
                            []
                   )
            )
            config.content
        ]


paddingToStyle : Padding -> Style
paddingToStyle padding =
    case padding of
        SmallPadding ->
            Css.padding2 (Css.px 10) (Css.px 13)

        NormalPadding ->
            Css.padding2 (Css.px 20) (Css.px 20)


eventsForTrigger : Trigger -> (Bool -> msg) -> List (Attribute msg)
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
containerPositioningForArrowPosition : Position -> List Style
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


pointerBox : Position -> Attribute msg
pointerBox position =
    css
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
        [ css
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


arrowInPosition : Position -> Style
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
