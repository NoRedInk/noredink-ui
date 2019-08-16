module Nri.Tooltip.V1 exposing
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

        tooltip
        |> withPadding SmallPadding
        |> withWidth FitToContent
        |> asPrimaryLabel {
            trigger : Trigger
            , triggerHtml : Html msg
            , onTrigger : Bool -> msg
            , isOpen : Bool
        }


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

import Accessibility.Styled as Html exposing (Attribute, Html, text)
import Accessibility.Styled.Role as Role
import Css exposing (Color, Style)
import Css.Global as Global
import EventExtras.Styled as Events
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts


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

You should probably use `OnHover` unless your tooltip has a link that someone
needs to click.

-}
type Trigger
    = OnHover
    | OnClick


{-| Used when the content of the tooltip is the "primary label" for its content. The tooltip content
will supercede the content of the trigger HTML for screen readers.

Here's what the fields in the configuration record do:

  - `trigger`: How do you open this tooltip?
  - `triggerHtml`: What element do you interact with to open the tooltip?
  - `onTrigger`: What `msg` should I send when the tooltip should open and
    close? The `Bool` represents the next `isOpen` value.
  - `isOpen`: Is the tooltip open now? (keep track of this in your model
    somewhere)

-}
primaryLabel :
    { trigger : Trigger
    , triggerHtml : Html msg
    , onTrigger : Bool -> msg
    , isOpen : Bool
    }
    -> Tooltip msg
    -> Html msg
primaryLabel =
    view


{-| Used when the content of the tooltip provides an "auxillary description" for its content.
-}
auxillaryDescription :
    { trigger : Trigger
    , triggerHtml : Html msg
    , onTrigger : Bool -> msg
    , isOpen : Bool
    }
    -> Tooltip msg
    -> Html msg
auxillaryDescription =
    view


{-| Supplementary information triggered by a "?" icon
-}
toggleTip :
    { trigger : Trigger
    , triggerHtml : Html msg
    , onTrigger : Bool -> msg
    , isOpen : Bool
    }
    -> Tooltip msg
    -> Html msg
toggleTip =
    view


{-| Here's what the fields in the configuration record do:

  - `trigger`: How do you open this tooltip?
  - `triggerHtml`: What element do you interact with to open the tooltip?
  - `onTrigger`: What `msg` should I send when the tooltip should open and
    close? The `Bool` represents the next `isOpen` value.
  - `isOpen`: Is the tooltip open now? (keep track of this in your model
    somewhere)

-}
view :
    { trigger : Trigger
    , triggerHtml : Html msg
    , onTrigger : Bool -> msg
    , isOpen : Bool
    }
    -> Tooltip msg
    -> Html msg
view { trigger, triggerHtml, onTrigger, isOpen } tooltip_ =
    Html.button
        ([ css
            [ Css.display Css.inlineBlock
            , Css.textAlign Css.left
            , Css.position Css.relative
            ]
         ]
            ++ eventsForTrigger trigger onTrigger
        )
        [ Html.div
            [ css [ Css.cursor Css.pointer, Css.displayFlex ] ]
            [ triggerHtml ]

        -- if we display the click-to-close overlay on hover, you will have to
        -- close the overlay by moving the mouse out of the window or clicking.
        , viewIf (\_ -> viewCloseTooltipOverlay (onTrigger False)) (isOpen && trigger == OnClick)

        -- Popout is rendered after the overlay, to allow client code to give it
        -- priority when clicking by setting its position
        , viewIf (\_ -> viewTooltip tooltip_) isOpen
        ]


{-| TODO: move me somewhere?
-}
viewIf : (() -> Html msg) -> Bool -> Html msg
viewIf viewFn condition =
    case condition of
        True ->
            viewFn ()

        False ->
            Html.text ""



-- INTERNALS


viewTooltip : Tooltip msg -> Html msg
viewTooltip (Tooltip config) =
    Html.div (containerPositioningForArrowPosition config.position)
        [ Html.aside
            [ css
                ([ Css.borderRadius (Css.px 8)
                 , case config.width of
                    Exactly width ->
                        Css.width (Css.px (toFloat width))

                    FitToContent ->
                        Css.whiteSpace Css.noWrap
                 , paddingToStyle config.padding

                 -- , Nri.ZIndex.global Css.absolute .tooltip -- TODO: copy this over
                 ]
                    ++ config.tooltipStyleOverrides
                )
            , pointerBox config.position

            -- We need to keep this animation in tests to make it pass: check out
            -- the NoAnimations middleware. So if you change the name here, please
            -- change that as well
            , Attrs.class "dont-disable-animation"
            , Role.toolTip
            ]
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
            [ Events.onClickStopPropagation (msg True) ]

        OnHover ->
            [ Events.onMouseEnter (msg True)
            , Events.onMouseLeave (msg False)
            , Events.onFocus (msg True)
            , Events.onBlur (msg False)
            ]


arrowSize : Float
arrowSize =
    8


tooltipColor : Color
tooltipColor =
    Colors.navy


{-| This returns an absolute positioning style attribute for the popout container for a given arrow position.
-}
containerPositioningForArrowPosition : Position -> List (Attribute msg)
containerPositioningForArrowPosition arrowPosition =
    List.map (\( k, v ) -> Attrs.style k v) <|
        case arrowPosition of
            OnTop ->
                [ ( "left", "50%" )
                , ( "top", "calc(-" ++ String.fromFloat arrowSize ++ "px - 2px)" )
                , ( "position", "absolute" )
                ]

            OnBottom ->
                [ ( "left", "50%" )
                , ( "bottom", "calc(-" ++ String.fromFloat arrowSize ++ "px - 2px)" )
                , ( "position", "absolute" )
                ]

            OnLeft ->
                [ ( "top", "50%" )
                , ( "left", "calc(-" ++ String.fromFloat arrowSize ++ "px - 2px)" )
                , ( "position", "absolute" )
                ]

            OnRight ->
                [ ( "top", "50%" )
                , ( "right", "calc(-" ++ String.fromFloat arrowSize ++ "px - 2px)" )
                , ( "position", "absolute" )
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
            ]
        , Events.onClickStopPropagation msg
        ]
        []



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
