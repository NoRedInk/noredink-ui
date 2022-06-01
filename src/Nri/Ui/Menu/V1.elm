module Nri.Ui.Menu.V1 exposing
    ( view, Alignment(..), TitleWrapping(..)
    , iconButton
    , iconButtonWithMenu
    , iconLink
    , Entry, group, none, entry
    )

{-| A togglable menu view and related buttons.

<https://zpl.io/a75OrE2>


## Menu buttons

@docs view, Alignment, TitleWrapping
@docs iconButton
@docs iconButtonWithMenu
@docs iconLink


## Menu content

@docs Entry, group, none, entry

-}

import Accessibility.Styled.Aria as Aria exposing (controls)
import Accessibility.Styled.Role as Role
import Css exposing (..)
import Css.Global exposing (descendants)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (class, classList, css)
import Html.Styled.Events exposing (onClick, onMouseDown)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V1 as Tooltip exposing (Tooltip)
import Nri.Ui.UiIcon.V1 as UiIcon
import String.Extra



-- ENTRIES


{-| Represents zero or more entries within the menu content
-}
type Entry msg
    = Single (Html msg)
    | Batch String (List (Entry msg))
    | None


{-| Represents a group of entries with a named legend
-}
group : String -> List (Entry msg) -> Entry msg
group =
    Batch


{-| Represents a single entry
-}
entry : Html msg -> Entry msg
entry =
    Single


{-| Represents no entries. Useful for conditionals
-}
none : Entry msg
none =
    None


{-| Determines how we deal with long titles. Should we make the menu expand in
height to show the full title or truncate it instead?
-}
type TitleWrapping
    = WrapAndExpandTitle
    | TruncateTitle


{-| Whether the menu content sticks to the left or right side of the button
-}
type Alignment
    = Left
    | Right


{-| Configure the menu

  - `icon`: display a particular icon to the left of the title
  - `entries`: the entries of the menu
  - `title`: the text to display in the menu button
  - `onToggle`: a message to trigger when then menu wants to invert its open state
  - `hasBorder`: whether the menu button has a border
  - `alignment`: where the menu popover should appear relative to the button
  - `isDisabled`: whether the menu can be openned
  - `buttonWidth`: optionally fix the width of the button to a number of pixels
  - `menuWidth` : optionally fix the width of the popover
  - `id`: a unique string id for the menu elements

-}
view :
    { icon : Maybe Svg.Svg
    , entries : List (Entry msg)
    , title : String
    , isOpen : Bool
    , onToggle : Bool -> msg
    , hasBorder : Bool
    , alignment : Alignment
    , wrapping : TitleWrapping
    , isDisabled : Bool
    , buttonWidth : Maybe Int
    , menuWidth : Maybe Int
    , id : String
    }
    -> Html msg
view config =
    let
        buttonId =
            config.id ++ "-button"

        menuId =
            config.id ++ "-menu"
    in
    div (Attributes.id config.id :: styleContainer)
        [ if config.isOpen then
            div
                (onClick (config.onToggle False)
                    :: class "Nri-Menu-Overlay"
                    :: styleOverlay
                )
                []

          else
            Html.text ""
        , div styleInnerContainer
            [ Html.button
                [ classList [ ( "ToggleButton", True ), ( "WithBorder", config.hasBorder ) ]
                , css
                    [ Nri.Ui.Fonts.V1.baseFont
                    , fontSize (px 15)
                    , backgroundColor Colors.white
                    , border zero
                    , padding (px 4)
                    , textAlign left
                    , height (pct 100)
                    , fontWeight (int 600)
                    , cursor pointer
                    , disabled
                        [ opacity (num 0.4)
                        , cursor notAllowed
                        ]
                    , Css.batch <|
                        if config.hasBorder then
                            [ border3 (px 1) solid Colors.gray75
                            , borderBottom3 (px 3) solid Colors.gray75
                            , borderRadius (px 8)
                            , padding2 (px 10) (px 15)
                            ]

                        else
                            []
                    ]
                , onClick <| config.onToggle (not config.isOpen)
                , Attributes.disabled config.isDisabled
                , Aria.disabled config.isDisabled
                , Aria.hasMenuPopUp
                , Aria.expanded config.isOpen
                , controls [ menuId ]
                , Attributes.id buttonId
                , config.buttonWidth
                    -- TODO: don't set this value as an inline style unnecessarily
                    |> Maybe.map (\w -> Attributes.style "width" (String.fromInt w ++ "px"))
                    |> Maybe.withDefault AttributesExtra.none
                ]
                [ div styleButtonInner
                    [ viewTitle { icon = config.icon, wrapping = config.wrapping, title = config.title, dataDescription = config.id }
                    , viewArrow { isOpen = config.isOpen }
                    ]
                ]
            , viewDropdown
                { alignment = config.alignment
                , isOpen = config.isOpen
                , isDisabled = config.isDisabled
                , menuId = menuId
                , buttonId = buttonId
                , menuWidth = config.menuWidth
                , entries = config.entries
                }
            ]
        ]


viewArrow : { isOpen : Bool } -> Html msg
viewArrow { isOpen } =
    span
        [ classList [ ( "Arrow", True ), ( "Open", isOpen ) ]
        , css
            [ width (px 12)
            , height (px 7)
            , marginLeft (px 5)
            , color Colors.azure
            , Css.flexShrink (Css.num 0)
            , descendants
                [ Css.Global.svg [ display block ]
                ]
            , property "transform-origin" "center"
            , property "transition" "transform 0.1s"
            , if isOpen then
                transform (rotate (deg 180))

              else
                Css.batch []
            ]
        ]
        [ Svg.toHtml UiIcon.arrowDown ]


type alias TitleConfig =
    { icon : Maybe Svg.Svg
    , wrapping : TitleWrapping
    , title : String
    , dataDescription : String
    }


viewTitle : TitleConfig -> Html msg
viewTitle config =
    div styleTitle
        [ viewJust (\icon -> span styleIconContainer [ Svg.toHtml icon ])
            config.icon
        , span
            (case config.wrapping of
                WrapAndExpandTitle ->
                    [ Attributes.attribute "data-nri-description" config.dataDescription ]

                TruncateTitle ->
                    [ class "Truncated"
                    , css
                        [ whiteSpace noWrap
                        , overflow hidden
                        , textOverflow ellipsis
                        ]
                    ]
            )
            [ Html.text config.title ]
        ]


type alias DropdownConfig msg =
    { alignment : Alignment
    , isOpen : Bool
    , isDisabled : Bool
    , menuId : String
    , buttonId : String
    , menuWidth : Maybe Int
    , entries : List (Entry msg)
    }


viewDropdown : DropdownConfig msg -> Html msg
viewDropdown config =
    let
        contentVisible =
            config.isOpen && not config.isDisabled
    in
    div
        [ classList [ ( "Content", True ), ( "ContentVisible", contentVisible ) ]
        , styleContent contentVisible config.alignment
        , Role.menu
        , Aria.labelledBy config.buttonId
        , Attributes.id config.menuId
        , Aria.hidden (not config.isOpen)
        , config.menuWidth
            -- TODO: don't set this style inline unnecessarily
            |> Maybe.map (\w -> Attributes.style "width" (String.fromInt w ++ "px"))
            |> Maybe.withDefault AttributesExtra.none
        ]
        (List.map viewEntry config.entries)


viewEntry : Entry msg -> Html msg
viewEntry entry_ =
    case entry_ of
        Single child ->
            div (Role.menuItem :: styleMenuEntryContainer) [ child ]

        Batch title childList ->
            case childList of
                [] ->
                    Html.text ""

                _ ->
                    fieldset styleGroupContainer <|
                        legend styleGroupTitle
                            [ span styleGroupTitleText [ Html.text title ] ]
                            :: List.map viewEntry childList

        None ->
            Html.text ""


{-| Display an icon button consistent with menu button.
-}
iconButton :
    { icon : Svg.Svg
    , label : String
    , isTooltipOpen : Bool
    , onShowTooltip : Bool -> msg
    , onClick : msg
    , isDisabled : Bool
    , id : String
    }
    -> Html msg
iconButton config =
    let
        perhapsOnclick =
            if config.isDisabled then
                []

            else
                -- Uses onMouseDown instead of onClick to prevent a race condition with the tooltip that can make the menu un-openable for mysterious causes
                [ onMouseDown config.onClick ]
    in
    div styleIconButtonContainer
        [ tooltip [ text config.label ]
            |> Tooltip.primaryLabel
                { trigger = Tooltip.OnHover
                , onTrigger = config.onShowTooltip
                , isOpen = config.isTooltipOpen
                , triggerHtml =
                    button
                        ([ Attributes.id config.id
                         , class "IconButton"
                         , css
                            (disabled
                                [ opacity (num 0.4)
                                , cursor notAllowed
                                ]
                                :: buttonLinkResets
                            )
                         , Aria.disabled config.isDisabled
                         , Attributes.disabled config.isDisabled
                         , Aria.label config.label
                         ]
                            ++ perhapsOnclick
                        )
                        [ independentIcon config.icon
                        ]
                , extraButtonAttrs = []
                , id = config.id ++ "-tooltip"
                }
        ]


{-| Configure the icon button & menu

  - `icon`: the icon that will be used in the iconButton
  - `label`: the text to display in the tooltip
  - `isTooltipOpen`: whether the tooltip is open when hovering over the icon
  - `onShowTooltip`: the message for toggling the tooltip
  - `entries`: the entries of the menu
  - `isOpen`: whether the menu is opened
  - `onToggle`: a message to trigger when then menu wants to invert its open state
  - `alignment`: where the menu popover should appear relative to the button
  - `isDisabled`: whether the menu can be openned
  - `id`: a unique string id for the menu elements
  - `menuWidth` : optionally fix the width of the popover

-}
iconButtonWithMenu :
    { icon : Svg.Svg
    , label : String
    , isTooltipOpen : Bool
    , onShowTooltip : Bool -> msg
    , entries : List (Entry msg)
    , isOpen : Bool
    , onToggle : Bool -> msg
    , alignment : Alignment
    , isDisabled : Bool
    , menuWidth : Maybe Int
    , id : String
    }
    -> Html msg
iconButtonWithMenu config =
    let
        buttonId =
            config.id ++ "-button"

        menuId =
            config.id ++ "-menu"
    in
    div (Attributes.id config.id :: styleContainer)
        [ if config.isOpen then
            div
                (onClick (config.onToggle False)
                    :: Attributes.class "Nri-Menu-Overlay"
                    :: styleOverlay
                )
                []

          else
            Html.text ""
        , div styleInnerContainer
            [ iconButton
                { icon = config.icon
                , isDisabled = config.isDisabled
                , label = config.label
                , onClick = config.onToggle (not config.isOpen)
                , isTooltipOpen = config.isTooltipOpen
                , onShowTooltip = config.onShowTooltip
                , id = buttonId
                }
            , viewDropdown
                { alignment = config.alignment
                , isOpen = config.isOpen
                , isDisabled = config.isDisabled
                , menuId = menuId
                , buttonId = buttonId
                , menuWidth = config.menuWidth
                , entries = config.entries
                }
            ]
        ]


{-| Display an icon link consistent with menu button styles.
-}
iconLink :
    { icon : Svg.Svg
    , label : String
    , isTooltipOpen : Bool
    , onShowTooltip : Bool -> msg
    , linkUrl : String
    , isDisabled : Bool
    }
    -> Html msg
iconLink config =
    let
        perhapsHref =
            if config.isDisabled then
                []

            else
                [ Attributes.href config.linkUrl ]
    in
    div styleIconButtonContainer
        [ tooltip [ text config.label ]
            |> Tooltip.primaryLabel
                { trigger = Tooltip.OnHover
                , onTrigger = config.onShowTooltip
                , isOpen = config.isTooltipOpen
                , triggerHtml =
                    a
                        ([ class "IconLink"
                         , css
                            (buttonLinkResets
                                ++ (if config.isDisabled then
                                        [ opacity (num 0.4)
                                        , cursor notAllowed
                                        ]

                                    else
                                        []
                                   )
                            )
                         , Aria.disabled config.isDisabled
                         , Attributes.id (String.Extra.dasherize config.label)
                         , Aria.label config.label
                         ]
                            ++ perhapsHref
                        )
                        [ independentIcon config.icon
                        ]
                , extraButtonAttrs = []
                , id = String.replace " " "-" config.label ++ "-tooltip"
                }
        ]


independentIcon : Svg -> Html msg
independentIcon icon =
    icon
        |> Svg.withColor Colors.azure
        |> Svg.withWidth (Css.px 25)
        |> Svg.withHeight (Css.px 25)
        |> Svg.withCss [ Css.margin (Css.px 10) ]
        |> Svg.toHtml



-- STYLES


buttonLinkResets : List Style
buttonLinkResets =
    [ boxSizing borderBox
    , border zero
    , padding zero
    , margin zero
    , backgroundColor transparent
    , cursor pointer
    , display inlineBlock
    , verticalAlign middle
    ]


tooltip : List (Html msg) -> Tooltip msg
tooltip =
    Tooltip.tooltip
        >> Tooltip.withPosition Tooltip.OnTop
        >> Tooltip.withWidth Tooltip.FitToContent
        >> Tooltip.withPadding Tooltip.SmallPadding


{-| -}
styleInnerContainer : List (Attribute msg)
styleInnerContainer =
    [ class "InnerContainer"
    , css [ position relative ]
    ]


styleOverlay : List (Attribute msg)
styleOverlay =
    [ class "Overlay"
    , css
        [ position fixed
        , width (pct 100)
        , height (pct 100)
        , left zero
        , top zero
        , zIndex (int 1)
        ]
    ]


styleMenuEntryContainer : List (Attribute msg)
styleMenuEntryContainer =
    [ class "MenuEntryContainer"
    , css
        [ padding2 (px 5) zero
        , position relative
        , firstChild
            [ paddingTop zero ]
        , lastChild
            [ paddingBottom zero ]
        ]
    ]


styleTitle : List (Attribute msg)
styleTitle =
    [ class "Title"
    , css
        [ width (pct 100)
        , overflow hidden
        , Css.displayFlex
        , Css.alignItems Css.center
        , color Colors.gray20
        ]
    ]


styleGroupTitle : List (Attribute msg)
styleGroupTitle =
    [ class "GroupTitle"
    , css
        [ Nri.Ui.Fonts.V1.baseFont
        , fontSize (px 12)
        , color Colors.gray45
        , margin zero
        , padding2 (px 5) zero
        , lineHeight initial
        , borderBottom zero
        , position relative
        , before
            [ property "content" "\"\""
            , width (pct 100)
            , backgroundColor Colors.gray75
            , height (px 1)
            , marginTop (px -1)
            , display block
            , top (pct 50)
            , position absolute
            ]
        ]
    ]


styleGroupTitleText : List (Attribute msg)
styleGroupTitleText =
    [ class "GroupTitleText"
    , css
        [ backgroundColor Colors.white
        , marginLeft (px 22)
        , padding2 zero (px 5)
        , zIndex (int 2)
        , position relative
        ]
    ]


styleGroupContainer : List (Attribute msg)
styleGroupContainer =
    [ class "GroupContainer"
    , css
        [ margin zero
        , padding zero
        , paddingBottom (px 15)
        , lastChild
            [ paddingBottom zero ]
        ]
    ]


styleButtonInner : List (Attribute msg)
styleButtonInner =
    [ class "ButtonInner"
    , css
        [ Css.displayFlex
        , Css.justifyContent Css.spaceBetween
        , Css.alignItems Css.center
        ]
    ]


styleIconContainer : List (Attribute msg)
styleIconContainer =
    [ class "IconContainer"
    , css
        [ width (px 21)
        , height (px 21)
        , marginRight (px 5)
        , display inlineBlock
        , Css.flexShrink (Css.num 0)
        , color Colors.azure
        ]
    ]


styleContent : Bool -> Alignment -> Attribute msg
styleContent contentVisible alignment =
    css
        [ padding (px 25)
        , border3 (px 1) solid Colors.gray85
        , minWidth (px 202)
        , position absolute
        , borderRadius (px 8)
        , marginTop (px 10)
        , zIndex (int 2)
        , backgroundColor Colors.white
        , listStyle Css.none
        , Css.property "box-shadow" "0 0 10px 0 rgba(0,0,0,0.1)"
        , zIndex (int 2)
        , before
            [ property "content" "\"\""
            , position absolute
            , top (px -12)
            , border3 (px 6) solid transparent
            , borderBottomColor Colors.gray85
            ]
        , after
            [ property "content" "\"\""
            , position absolute
            , top (px -10)
            , zIndex (int 2)
            , border3 (px 5) solid transparent
            , borderBottomColor Colors.white
            ]
        , case alignment of
            Left ->
                Css.batch
                    [ left zero
                    , before [ left (px 19) ]
                    , after [ left (px 20) ]
                    ]

            Right ->
                Css.batch
                    [ right zero
                    , before [ right (px 19) ]
                    , after [ right (px 20) ]
                    ]
        , if contentVisible then
            display block

          else
            display Css.none
        ]


styleContainer : List (Attribute msg)
styleContainer =
    [ class "Container"
    , css
        [ position relative
        , display inlineBlock
        ]
    ]


styleIconButtonContainer : List (Attribute msg)
styleIconButtonContainer =
    [ class "IconButtonContainer"
    , css
        [ display inlineBlock
        , position relative
        ]
    ]
