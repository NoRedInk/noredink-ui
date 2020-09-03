module Nri.Ui.Tooltip.V2 exposing
    ( view, toggleTip
    , Attribute
    , plaintext, html
    , onTop, onBottom, onLeft, onRight
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , onClick, onHover
    , open
    , css, custom, customTriggerAttributes
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
  - move the onTrigger event to the attributes
  - extraButtonAttrs becomes attribute `customTriggerAttributes`
  - isOpen field becomes the `open` attribute
  - fold toggleTip and view into each other, so there's less to maintain

These tooltips follow the accessibility recommendations from: <https://inclusive-components.design/tooltips-toggletips>

Example usage:

        Tooltip.view
            { triggerHtml = text "Click me to open the tooltip"
            , id = "my-tooltip"
            }
            [ Tooltip.plaintext "Gradebook"
            , Tooltip.primaryLabel
            , Tooltip.onClick MyOnTriggerMsg
            , Tooltip.open True
            ]


## Suggested Improvements for V2

  - The toggle tip does not currently manage focus correctly for keyboard users - if a
    user tries to click on a link in the toggle tip, the tip will disappear as focus moves
    to the next item in the page. This should be improved in the next release.

@docs view, toggleTip
@docs Attribute
@docs plaintext, html
@docs onTop, onBottom, onLeft, onRight
@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs onClick, onHover
@docs open
@docs css, custom, customTriggerAttributes
@docs primaryLabel, auxillaryDescription

-}

import Accessibility.Styled as Html exposing (Attribute, Html, text)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (Color, Style)
import Css.Global as Global
import EventExtras
import Html.Styled as Root
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import String.Extra


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
            { position = OnTop
            , content = []
            , attributes = []
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


{-| Use this helper to add custom attributes to the tooltip trigger.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
customTriggerAttributes : List (Html.Attribute msg) -> Attribute msg
customTriggerAttributes attributes =
    Attribute (\config -> { config | triggerAttributes = attributes })


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

  - `triggerHtml`: What element do you interact with to open the tooltip?
  - `id`: A unique identifier used to associate the trigger with its content

-}
view : { triggerHtml : Html msg, id : String } -> List (Attribute msg) -> Html msg
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
        { triggerHtml =
            UiIcon.help
                |> Svg.withWidth (Css.px 20)
                |> Svg.withHeight (Css.px 20)
                |> Svg.withColor Colors.azure
                |> Svg.withLabel label
                |> Svg.toHtml
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ -- Take up enough room within the document flow
                          Css.width (Css.px 20)
                        , Css.height (Css.px 20)
                        , Css.margin (Css.px 5)
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
    { triggerHtml : Html msg
    , id : String -- Accessibility: Used to match tooltip to trigger
    }
    -> Tooltip msg
    -> Html msg
viewTooltip_ { triggerHtml, id } tooltip_ =
    let
        ( containerEvents, buttonEvents ) =
            case tooltip_.trigger of
                Just (OnClick msg) ->
                    ( []
                    , [ EventExtras.onClickStopPropagation (msg True)
                      , Events.onFocus (msg True)
                      , Events.onBlur (msg False)
                      ]
                    )

                Just (OnHover msg) ->
                    ( [ Events.onMouseEnter (msg True)
                      , Events.onMouseLeave (msg False)
                      ]
                    , [ Events.onFocus (msg True)
                      , Events.onBlur (msg False)
                      , EventExtras.onClickStopPropagation (msg True)
                      ]
                    )

                Nothing ->
                    ( [], [] )
    in
    Nri.Ui.styled Html.div
        "Nri-Ui-Tooltip-V2"
        tooltipContainerStyles
        []
        [ Root.div containerEvents
            [ Html.button
                ([ if tooltip_.isOpen then
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
                    ++ buttonEvents
                    ++ tooltip_.triggerAttributes
                )
                [ triggerHtml
                , hoverBridge tooltip_
                ]
            , viewTooltip (Just id) tooltip_
            ]
        ]


{-| This is a "bridge" for the cursor to move from trigger content to tooltip, so the user can click on links, etc.
-}
hoverBridge : Tooltip msg -> Html msg
hoverBridge { isOpen, position } =
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
                case position of
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


viewTooltip : Maybe String -> Tooltip msg -> Html msg
viewTooltip maybeTooltipId config =
    if config.isOpen then
        viewOpenTooltip maybeTooltipId config

    else
        text ""


viewOpenTooltip : Maybe String -> Tooltip msg -> Html msg
viewOpenTooltip maybeTooltipId config =
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
