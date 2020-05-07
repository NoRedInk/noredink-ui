module Nri.Menu exposing
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
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Extra exposing (Snippet, classSelector, classSelectors)
import Css.Global exposing (descendants)
import Data.Url as Url exposing (Url)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes.Extra
import Html.Styled.Events exposing (onClick, onMouseDown)
import Html.Styled.Extra exposing (viewJust)
import Nri.Tooltip as Tooltip exposing (Tooltip)
import Nri.Ui.Colors.V1
import Nri.Ui.Fonts.V1
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import Util



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
    div
        [ class [ Container ]
        , Attr.id config.id
        ]
        [ if config.isOpen then
            div
                [ class [ Overlay ]
                , Attr.class "Nri-Menu-Overlay"
                , onClick (config.onToggle False)
                ]
                []

          else
            Html.text ""
        , div [ class [ InnerContainer ] ]
            [ Html.button
                [ classList [ ( ToggleButton, True ), ( WithBorder, config.hasBorder ) ]
                , onClick <| config.onToggle (not config.isOpen)
                , Attr.disabled config.isDisabled
                , Widget.disabled config.isDisabled
                , Widget.hasMenuPopUp
                , Widget.expanded config.isOpen
                , controls menuId
                , Attr.id buttonId
                , config.buttonWidth
                    |> Maybe.map (\w -> Attr.style "width" (String.fromInt w ++ "px"))
                    |> Maybe.withDefault Html.Styled.Attributes.Extra.none
                ]
                [ div [ class [ ButtonInner ] ]
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
        [ Attr.classList [ ( "Arrow", True ), ( "Open", isOpen ) ]
        , Attr.css
            [ width (px 12)
            , height (px 7)
            , marginLeft (px 5)
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
    div [ class [ Title ] ]
        [ viewJust
            (\icon ->
                span [ class [ IconContainer ] ]
                    [ Svg.toHtml icon ]
            )
            config.icon
        , span
            [ classList
                [ ( Truncated
                  , case config.wrapping of
                        WrapAndExpandTitle ->
                            False

                        TruncateTitle ->
                            True
                  )
                ]
            , Attr.attribute "data-nri-description" config.dataDescription
            ]
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
    div
        [ classList
            [ ( Content, True )
            , ( Align config.alignment, True )
            , ( ContentVisible, config.isOpen && not config.isDisabled )
            ]
        , Role.menu
        , Aria.labelledBy config.buttonId
        , Attr.id config.menuId
        , Widget.hidden (not config.isOpen)
        , config.menuWidth
            |> Maybe.map (\w -> Attr.style "width" (String.fromInt w ++ "px"))
            |> Maybe.withDefault Html.Styled.Attributes.Extra.none
        ]
        (List.map viewEntry config.entries)


viewEntry : Entry msg -> Html msg
viewEntry entry_ =
    case entry_ of
        Single child ->
            div
                [ class [ MenuEntryContainer ]
                , Role.menuItem
                ]
                [ child ]

        Batch title childList ->
            case childList of
                [] ->
                    Html.text ""

                _ ->
                    fieldset [ class [ GroupContainer ] ] <|
                        legend [ class [ GroupTitle ] ]
                            [ span [ class [ GroupTitleText ] ]
                                [ Html.text title ]
                            ]
                            :: List.map viewEntry childList

        None ->
            Html.text ""


{-| Display an icon button consistent with menu button
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
    div [ class [ IconButtonContainer ] ]
        [ tooltip [ text config.label ]
            |> Tooltip.view
                { trigger = Tooltip.OnHover
                , onTrigger = config.onShowTooltip
                , isOpen = config.isTooltipOpen
                , triggerHtml =
                    button
                        ([ class [ IconButton ]
                         , Attr.id config.id
                         , Widget.disabled config.isDisabled
                         , Attr.disabled config.isDisabled
                         , Widget.label config.label
                         ]
                            ++ perhapsOnclick
                        )
                        [ Svg.toHtml config.icon
                        ]
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
    div
        [ class [ Container ]
        , Attr.id config.id
        ]
        [ if config.isOpen then
            div
                [ class [ Overlay ]
                , Attr.class "Nri-Menu-Overlay"
                , onClick (config.onToggle False)
                ]
                []

          else
            Html.text ""
        , div [ class [ InnerContainer ] ]
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
    , linkUrl : Url
    , isDisabled : Bool
    }
    -> Html msg
iconLink config =
    let
        perhapsHref =
            if config.isDisabled then
                []

            else
                [ Attr.href <| Url.toString config.linkUrl ]
    in
    div [ class [ IconButtonContainer ] ]
        [ tooltip [ text config.label ]
            |> Tooltip.view
                { trigger = Tooltip.OnHover
                , onTrigger = config.onShowTooltip
                , isOpen = config.isTooltipOpen
                , triggerHtml =
                    a
                        ([ classList
                            [ ( IconLink, True )
                            , ( Disabled, config.isDisabled ) -- `a` tags don't allow the `disabled` attribute so we have to use a class to apply disabled styles
                            ]
                         , Widget.disabled config.isDisabled
                         , Attr.id (Util.dashify config.label)
                         , Widget.label config.label
                         ]
                            ++ perhapsHref
                        )
                        [ Svg.toHtml config.icon
                        ]
                }
        ]



-- STYLES


tooltip : List (Html msg) -> Tooltip msg
tooltip =
    Tooltip.tooltip
        >> Tooltip.withPosition Tooltip.OnTop
        >> Tooltip.withWidth Tooltip.FitToContent
        >> Tooltip.withPadding Tooltip.SmallPadding


type CssClass
    = ToggleButton
    | ButtonInner
    | IconContainer
    | Content
    | Container
    | WithBorder
    | Align Alignment
    | Title
    | Truncated
    | ContentVisible
    | MenuEntryContainer
    | GroupContainer
    | GroupTitle
    | GroupTitleText
    | Overlay
    | InnerContainer
    | IconButtonContainer
    | IconButton
    | IconLink
    | Disabled


{-| -}
snippets : List (Snippet CssClass)
snippets =
    [ classSelector InnerContainer
        [ position relative
        ]
    , classSelector Overlay
        [ position fixed
        , width (pct 100)
        , height (pct 100)
        , left zero
        , top zero
        , zIndex (int 1)
        ]
    , classSelector MenuEntryContainer
        [ padding2 (px 5) zero
        , position relative
        , firstChild
            [ paddingTop zero ]
        , lastChild
            [ paddingBottom zero ]
        ]
    , classSelector Title
        [ width (pct 100)
        , overflow hidden
        , Css.displayFlex
        , Css.alignItems Css.center
        ]
    , classSelector Truncated
        [ whiteSpace noWrap
        , overflow hidden
        , textOverflow ellipsis
        ]
    , classSelector GroupTitle
        [ Nri.Ui.Fonts.V1.baseFont
        , fontSize (px 12)
        , color Nri.Ui.Colors.V1.gray45
        , margin zero
        , padding2 (px 5) zero
        , lineHeight initial
        , borderBottom zero
        , position relative
        , before
            [ property "content" "\"\""
            , width (pct 100)
            , backgroundColor Nri.Ui.Colors.V1.gray75
            , height (px 1)
            , marginTop (px -1)
            , display block
            , top (pct 50)
            , position absolute
            ]
        ]
    , classSelector GroupTitleText
        [ backgroundColor Nri.Ui.Colors.V1.white
        , marginLeft (px 22)
        , padding2 zero (px 5)
        , zIndex (int 2)
        , position relative
        ]
    , classSelector GroupContainer
        [ margin zero
        , padding zero
        , paddingBottom (px 15)
        , lastChild
            [ paddingBottom zero ]
        ]
    , classSelector ToggleButton
        [ Nri.Ui.Fonts.V1.baseFont
        , fontSize (px 15)
        , color Nri.Ui.Colors.V1.azure
        , backgroundColor Nri.Ui.Colors.V1.white
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
        ]
    , classSelectors [ ToggleButton, WithBorder ]
        [ border3 (px 1) solid Nri.Ui.Colors.V1.gray75
        , borderBottom3 (px 3) solid Nri.Ui.Colors.V1.gray75
        , borderRadius (px 8)
        , padding2 (px 10) (px 15)
        ]
    , classSelector ButtonInner
        [ Css.displayFlex
        , Css.justifyContent Css.spaceBetween
        , Css.alignItems Css.center
        ]
    , classSelector IconContainer
        [ width (px 21)
        , height (px 21)
        , marginRight (px 5)
        , display inlineBlock
        , Css.flexShrink (Css.num 0)
        ]
    , classSelector Content
        [ padding (px 25)
        , border3 (px 1) solid Nri.Ui.Colors.V1.azure
        , minWidth (px 202)
        , position absolute
        , borderRadius (px 8)
        , marginTop (px 10)
        , zIndex (int 2)
        , backgroundColor Nri.Ui.Colors.V1.white
        , display Css.none
        , listStyle Css.none
        , zIndex (int 2)
        , before
            [ property "content" "\"\""
            , position absolute
            , top (px -12)
            , border3 (px 6) solid transparent
            , borderBottomColor Nri.Ui.Colors.V1.azure
            ]
        , after
            [ property "content" "\"\""
            , position absolute
            , top (px -10)
            , zIndex (int 2)
            , border3 (px 5) solid transparent
            , borderBottomColor Nri.Ui.Colors.V1.white
            ]
        ]
    , classSelectors [ Content, Align Left ]
        [ left zero
        , before [ left (px 19) ]
        , after [ left (px 20) ]
        ]
    , classSelectors [ Content, Align Right ]
        [ right zero
        , before [ right (px 19) ]
        , after [ right (px 20) ]
        ]
    , classSelectors [ Content, ContentVisible ]
        [ display block ]
    , classSelector Container
        [ position relative
        , display inlineBlock
        ]
    , classSelector IconButtonContainer
        [ display inlineBlock
        , position relative
        ]
    , classSelector IconButton
        [ border zero
        , backgroundColor transparent
        , color Nri.Ui.Colors.V1.azure
        , width (px 45)
        , height (px 45) -- Matches Nri.Button.Medium height
        , padding (px 10)
        , cursor pointer

        -- NOTE: no hover state is necessary because the tooltip appears on hover
        , disabled
            [ opacity (num 0.4)
            , cursor notAllowed
            ]
        ]
    , classSelector IconLink
        [ border zero
        , backgroundColor transparent
        , color Nri.Ui.Colors.V1.azure
        , width (px 45)
        , height (px 45) -- Matches Nri.Button.Medium height
        , padding (px 10)
        , cursor pointer
        , display inlineBlock
        , verticalAlign middle

        -- NOTE: override default link hover styling, no hover state is necessary because the tooltip appears on hover
        , hover
            [ color Nri.Ui.Colors.V1.azure
            ]
        ]
    , classSelectors [ IconLink, Disabled ]
        [ opacity (num 0.4)
        , cursor notAllowed
        ]
    ]


{-| -}
class : List CssClass -> Html.Attribute msg
class =
    Css.Extra.class snippets


{-| -}
classList : List ( CssClass, Bool ) -> Html.Attribute msg
classList =
    Css.Extra.classList snippets
