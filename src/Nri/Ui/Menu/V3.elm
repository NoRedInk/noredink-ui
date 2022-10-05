module Nri.Ui.Menu.V3 exposing
    ( view, button, custom, Config
    , Attribute, Button, ButtonAttribute
    , alignment, isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover, disclosure
    , Alignment(..)
    , icon, wrapping, hasBorder, buttonWidth
    , TitleWrapping(..)
    , Entry, group, entry
    )

{-| Patch changes:

  - Use `Shadows`

Changes from V2:

  - Adpoted attribute pattern
  - Added option to customize the z-index

A togglable menu view and related buttons.

<https://zpl.io/a75OrE2>


## Menu rendering

@docs view, button, custom, Config
@docs Attribute, Button, ButtonAttribute


## Menu attributes

@docs alignment, isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover, disclosure
@docs Alignment


## Button attributes

@docs icon, wrapping, hasBorder, buttonWidth
@docs TitleWrapping


## Menu content

@docs Entry, group, entry

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css exposing (..)
import Css.Global exposing (descendants)
import EventExtras exposing (onKeyDownPreventDefault)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (class, classList, css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon
import Nri.Ui.WhenFocusLeaves.V1 as WhenFocusLeaves


{-| -}
type ButtonAttribute
    = ButtonAttribute (ButtonConfig -> ButtonConfig)


{-| -}
type Attribute msg
    = Attribute (MenuConfig msg -> MenuConfig msg)


{-| -}
type Button msg
    = StandardButton (MenuConfig msg -> List (Html.Attribute msg) -> Html msg)
    | CustomButton (List (Html.Attribute msg) -> Html msg)


{-| -}
type alias Config msg =
    { button : Button msg
    , entries : List (Entry msg)
    , isOpen : Bool
    , focusAndToggle : { isOpen : Bool, focus : Maybe String } -> msg
    }


type alias MenuConfig msg =
    { -- These come from Config
      button : Button msg
    , entries : List (Entry msg)
    , isOpen : Bool
    , focusAndToggle : { isOpen : Bool, focus : Maybe String } -> msg

    -- These are set using Attribute
    , alignment : Alignment
    , isDisabled : Bool
    , menuWidth : Maybe Int
    , buttonId : String
    , menuId : String
    , zIndex : Int
    , opensOnHover : Bool
    , purpose : Purpose
    }


type alias ButtonConfig =
    { icon : Maybe Svg.Svg
    , wrapping : TitleWrapping
    , hasBorder : Bool
    , buttonWidth : Maybe Int
    }


type Purpose
    = NavMenu
    | Disclosure { lastId : String }



-- Generators for ButtonAttribute


{-| Display a particular icon to the left of the title
-}
icon : Svg.Svg -> ButtonAttribute
icon svg =
    ButtonAttribute <| \config -> { config | icon = Just svg }


{-| Determines how we deal with long titles. If not specified it defaults to `WrapAndExpandTitle`.
-}
wrapping : TitleWrapping -> ButtonAttribute
wrapping value =
    ButtonAttribute <| \config -> { config | wrapping = value }


{-| Whether the menu button has a border. If not specified it defaults to `True`.
-}
hasBorder : Bool -> ButtonAttribute
hasBorder value =
    ButtonAttribute <| \config -> { config | hasBorder = value }


{-| Fix the width of the button to a number of pixels
-}
buttonWidth : Int -> ButtonAttribute
buttonWidth value =
    ButtonAttribute <| \config -> { config | buttonWidth = Just value }



-- Generators for Attribute


{-| Where the menu popover should appear relative to the button
-}
alignment : Alignment -> Attribute msg
alignment value =
    Attribute <| \config -> { config | alignment = value }


{-| Whether the menu can be openned
-}
isDisabled : Bool -> Attribute msg
isDisabled value =
    Attribute <| \config -> { config | isDisabled = value }


{-| Fix the width of the popover |
-}
menuWidth : Int -> Attribute msg
menuWidth value =
    Attribute <| \config -> { config | menuWidth = Just value }


{-| A unique string identifier for the button that opens/closes the menu
-}
buttonId : String -> Attribute msg
buttonId value =
    Attribute <| \config -> { config | buttonId = value }


{-| A unique string identifier for the menu
-}
menuId : String -> Attribute msg
menuId value =
    Attribute <| \config -> { config | menuId = value }


{-| The CSS `z-index` used to render the menu. Defaults to `1`.
-}
menuZIndex : Int -> Attribute msg
menuZIndex value =
    Attribute <| \config -> { config | zIndex = value }


{-| Whether the menu will be opened/closed by mouseEnter and mouseLeave interaction. Defaults to `False`.
-}
opensOnHover : Bool -> Attribute msg
opensOnHover value =
    Attribute <| \config -> { config | opensOnHover = value }


{-| Makes the menu behave as a disclosure.

For more information, please read [Disclosure (Show/Hide) pattern](https://www.w3.org/WAI/ARIA/apg/patterns/disclosure/).

You will need to pass in the last focusable element in the disclosed content in order for:

  - any focusable elements in the disclosed content to be keyboard accessible
  - the disclosure to close appropriately when the user tabs past all of the disclosed content

-}
disclosure : { lastId : String } -> Attribute msg
disclosure exitFocusManager =
    Attribute (\config -> { config | purpose = Disclosure exitFocusManager })


{-| Menu/pulldown configuration:

  - `attributes`: List of (attributes)[#menu-attributes] to apply to the menu.
  - `config`: Configuration parameters:
      - `button`: the `Button` to open the menu
      - `entries`: the entries of the menu
      - `isOpen`: whether the menu is currently open or not
      - `focusAndToggle`: the message produced to control the open/closed state and DOM focus

-}
view : List (Attribute msg) -> Config msg -> Html msg
view attributes config =
    let
        default =
            { button = config.button
            , entries = config.entries
            , isOpen = config.isOpen
            , focusAndToggle = config.focusAndToggle
            , alignment = Right
            , isDisabled = False
            , menuWidth = Nothing
            , buttonId = ""
            , menuId = ""
            , zIndex = 1
            , opensOnHover = False
            , purpose = NavMenu
            }

        menuConfig =
            attributes
                |> List.foldl (\(Attribute attr) c -> attr c) default
    in
    viewCustom menuConfig



-- ENTRIES


{-| Represents zero or more entries within the menu content
-}
type Entry msg
    = Single String (List (Html.Attribute msg) -> Html msg)
    | Batch String (List (Entry msg))


{-| Represents a group of entries with a named legend.
-}
group : String -> List (Entry msg) -> Entry msg
group legendName entries =
    Batch legendName entries


{-| Represents a single **focusable** entry.

Pass in the id you'd like for your menu item, which will be used to manage the focus.

    Menu.entry "my-button-id"
        (\attributes -> Button.button "One great button" [ Button.custom attributes ])

-}
entry : String -> (List (Html.Attribute msg) -> Html msg) -> Entry msg
entry id =
    Single id


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


{-| Defines a standard `Button` for the menu
-}
button : List ButtonAttribute -> String -> Button msg
button attributes title =
    let
        defaultButtonConfig =
            { icon = Nothing
            , wrapping = WrapAndExpandTitle
            , buttonWidth = Nothing
            , hasBorder = True
            }

        buttonConfig =
            attributes
                |> List.foldl (\(ButtonAttribute attr) config -> attr config) defaultButtonConfig
    in
    StandardButton
        (\menuConfig buttonAttributes ->
            Html.button
                ([ classList
                    [ ( "ToggleButton", True )
                    , ( "WithBorder", buttonConfig.hasBorder )
                    , ( FocusRing.customClass, True )
                    ]
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
                    , pseudoClass "focus-visible"
                        [ outline none
                        , FocusRing.boxShadows []
                        ]
                    , if menuConfig.isDisabled then
                        cursor notAllowed

                      else
                        Css.batch []
                    , Css.batch <|
                        if buttonConfig.hasBorder then
                            [ border3 (px 1) solid Colors.gray75
                            , if menuConfig.isDisabled then
                                backgroundColor Colors.gray85

                              else
                                borderBottom3 (px 3) solid Colors.gray75
                            , borderRadius (px 8)
                            , padding2 (px 10) (px 15)
                            ]

                        else
                            []
                    , Maybe.map (\w -> Css.width (Css.px (toFloat w))) buttonConfig.buttonWidth
                        |> Maybe.withDefault (Css.batch [])
                    ]
                 ]
                    ++ buttonAttributes
                )
                [ div styleButtonInner
                    [ viewTitle { icon = buttonConfig.icon, wrapping = buttonConfig.wrapping, title = title }
                    , viewArrow menuConfig
                    ]
                ]
        )


{-| Defines a custom `Button` for the menu
-}
custom : (List (Html.Attribute msg) -> Html msg) -> Button msg
custom builder =
    CustomButton builder


viewArrow : { config | isOpen : Bool, isDisabled : Bool } -> Html msg
viewArrow config =
    span
        [ classList [ ( "Arrow", True ), ( "Open", config.isOpen ) ]
        , css
            [ width (px 12)
            , height (px 7)
            , marginLeft (px 5)
            , color
                (if config.isDisabled then
                    Colors.gray20

                 else
                    Colors.azure
                )
            , Css.flexShrink (Css.num 0)
            , descendants
                [ Css.Global.svg [ display block ]
                ]
            , property "transform-origin" "center"
            , property "transition" "transform 0.4s"
            , if config.isOpen then
                transform (rotate (deg 180))

              else
                Css.batch []
            ]
        ]
        [ Svg.toHtml UiIcon.arrowDown ]


viewTitle :
    { icon : Maybe Svg.Svg
    , wrapping : TitleWrapping
    , title : String
    }
    -> Html msg
viewTitle config =
    div styleTitle
        [ viewJust (\iconSvg -> span styleIconContainer [ Svg.toHtml iconSvg ])
            config.icon
        , span
            (case config.wrapping of
                WrapAndExpandTitle ->
                    [ Attributes.attribute "data-nri-description" config.title ]

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


viewCustom : MenuConfig msg -> Html msg
viewCustom config =
    let
        ( maybeFirstFocusableElementId, maybeLastFocusableElementId ) =
            ( List.head (getFirstIds config.entries), List.head (getLastIds config.entries) )

        contentVisible =
            config.isOpen && not config.isDisabled
    in
    div
        (Attributes.id (config.buttonId ++ "__container")
            :: Key.onKeyDown
                (Key.escape
                    (config.focusAndToggle
                        { isOpen = False
                        , focus = Just config.buttonId
                        }
                    )
                    :: (case config.purpose of
                            NavMenu ->
                                [ Key.tab
                                    (config.focusAndToggle
                                        { isOpen = False
                                        , focus = Nothing
                                        }
                                    )
                                , Key.tabBack
                                    (config.focusAndToggle
                                        { isOpen = False
                                        , focus = Nothing
                                        }
                                    )
                                ]

                            Disclosure { lastId } ->
                                [ WhenFocusLeaves.toDecoder
                                    { firstId = config.buttonId
                                    , lastId = lastId
                                    , tabBackAction =
                                        config.focusAndToggle
                                            { isOpen = False
                                            , focus = Nothing
                                            }
                                    , tabForwardAction =
                                        config.focusAndToggle
                                            { isOpen = False
                                            , focus = Nothing
                                            }
                                    }
                                ]
                       )
                )
            :: styleContainer
        )
        [ if config.isOpen then
            div
                (Events.onClick
                    (config.focusAndToggle
                        { isOpen = False
                        , focus = Nothing
                        }
                    )
                    :: class "Nri-Menu-Overlay"
                    :: styleOverlay config
                )
                []

          else
            Html.text ""
        , div
            [ class "InnerContainer"
            , css
                [ position relative
                , if config.isOpen then
                    zIndex (int <| config.zIndex + 1)

                  else
                    Css.batch []
                ]
            , if not config.isDisabled && config.opensOnHover && config.isOpen then
                Events.onMouseLeave
                    (config.focusAndToggle
                        { isOpen = False
                        , focus = Nothing
                        }
                    )

              else
                AttributesExtra.none
            ]
            [ let
                buttonAttributes =
                    [ Aria.disabled config.isDisabled
                    , case config.purpose of
                        NavMenu ->
                            Aria.hasMenuPopUp

                        Disclosure _ ->
                            AttributesExtra.none
                    , Aria.expanded config.isOpen
                    , -- Whether the menu is open or closed, move to the
                      -- first menu item if the "down" arrow is pressed
                      -- as long as it's not a Disclosed
                      case ( config.purpose, maybeFirstFocusableElementId, maybeLastFocusableElementId ) of
                        ( NavMenu, Just firstFocusableElementId, Just lastFocusableElementId ) ->
                            Key.onKeyDownPreventDefault
                                [ Key.down
                                    (config.focusAndToggle
                                        { isOpen = True
                                        , focus = Just firstFocusableElementId
                                        }
                                    )
                                , Key.up
                                    (config.focusAndToggle
                                        { isOpen = True
                                        , focus = Just lastFocusableElementId
                                        }
                                    )
                                ]

                        _ ->
                            AttributesExtra.none
                    , Aria.controls [ config.menuId ]
                    , Attributes.id config.buttonId
                    , if config.isDisabled then
                        AttributesExtra.none

                      else
                        Events.custom "click"
                            (Json.Decode.succeed
                                { preventDefault = True
                                , stopPropagation = True
                                , message =
                                    config.focusAndToggle
                                        { isOpen = not config.isOpen
                                        , focus = Nothing
                                        }
                                }
                            )
                    , if not config.isDisabled && config.opensOnHover then
                        Events.onMouseEnter
                            (config.focusAndToggle
                                { isOpen = True
                                , focus = Nothing
                                }
                            )

                      else
                        AttributesExtra.none
                    ]
              in
              case config.button of
                StandardButton standardButton ->
                    standardButton config buttonAttributes

                CustomButton customButton ->
                    customButton buttonAttributes
            , div [ styleOuterContent contentVisible config ]
                [ div
                    [ AttributesExtra.nriDescription "menu-hover-bridge"
                    , css
                        [ Css.height (px 10)
                        ]
                    ]
                    []
                , div
                    [ classList [ ( "Content", True ), ( "ContentVisible", contentVisible ) ]
                    , styleContent contentVisible config
                    , case config.purpose of
                        NavMenu ->
                            Role.menu

                        Disclosure _ ->
                            AttributesExtra.none
                    , Aria.labelledBy config.buttonId
                    , Attributes.id config.menuId
                    , css
                        [ Maybe.map (\w -> Css.width (Css.px (toFloat w))) config.menuWidth
                            |> Maybe.withDefault (Css.batch [])
                        ]
                    ]
                    (viewEntries config
                        { focusAndToggle = config.focusAndToggle
                        , previousId = Maybe.withDefault "" maybeLastFocusableElementId
                        , nextId = Maybe.withDefault "" maybeFirstFocusableElementId
                        }
                        config.entries
                    )
                ]
            ]
        ]


getFirstIds : List (Entry msg) -> List String
getFirstIds entries =
    let
        getIdString elem =
            case elem of
                Single idString _ ->
                    Just idString

                Batch _ es ->
                    Maybe.andThen getIdString (List.head es)
    in
    List.filterMap getIdString entries


getLastIds : List (Entry msg) -> List String
getLastIds entries =
    let
        getIdString elem =
            case elem of
                Single idString _ ->
                    Just idString

                Batch _ es ->
                    Maybe.andThen getIdString (List.head (List.reverse es))
    in
    List.filterMap getIdString (List.reverse entries)


viewEntries :
    MenuConfig msg
    ->
        { focusAndToggle : { isOpen : Bool, focus : Maybe String } -> msg
        , previousId : String
        , nextId : String
        }
    -> List (Entry msg)
    -> List (Html msg)
viewEntries config { previousId, nextId, focusAndToggle } entries =
    let
        firstIds =
            getFirstIds entries

        lastIds =
            getLastIds entries
    in
    List.map3
        (\e upId downId ->
            viewEntry config
                focusAndToggle
                { upId = upId
                , downId = downId
                , entry_ = e
                }
        )
        entries
        (previousId :: List.reverse lastIds)
        (List.drop 1 firstIds ++ [ nextId ])


viewEntry :
    MenuConfig msg
    -> ({ isOpen : Bool, focus : Maybe String } -> msg)
    -> { upId : String, downId : String, entry_ : Entry msg }
    -> Html msg
viewEntry config focusAndToggle { upId, downId, entry_ } =
    case entry_ of
        Single id view_ ->
            div
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
                [ view_
                    [ Role.menuItem
                    , Attributes.id id
                    , Key.tabbable False
                    , onKeyDownPreventDefault
                        [ Key.up
                            (focusAndToggle
                                { isOpen = True
                                , focus = Just upId
                                }
                            )
                        , Key.down
                            (focusAndToggle
                                { isOpen = True
                                , focus = Just downId
                                }
                            )
                        ]
                    ]
                ]

        Batch title childList ->
            case childList of
                [] ->
                    Html.text ""

                _ ->
                    fieldset styleGroupContainer <|
                        legend styleGroupTitle
                            [ span (styleGroupTitleText config) [ Html.text title ] ]
                            :: viewEntries config
                                { focusAndToggle = focusAndToggle
                                , previousId = upId
                                , nextId = downId
                                }
                                childList



-- STYLES


styleOverlay : MenuConfig msg -> List (Html.Attribute msg)
styleOverlay config =
    [ class "Overlay"
    , css
        [ position fixed
        , width (pct 100)
        , height (pct 100)
        , left zero
        , top zero
        , zIndex (int config.zIndex)
        ]
    ]


styleTitle : List (Html.Attribute msg)
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


styleGroupTitle : List (Html.Attribute msg)
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


styleGroupTitleText : MenuConfig msg -> List (Html.Attribute msg)
styleGroupTitleText config =
    [ class "GroupTitleText"
    , css
        [ backgroundColor Colors.white
        , marginLeft (px 22)
        , padding2 zero (px 5)
        , zIndex (int <| config.zIndex + 1)
        , position relative
        ]
    ]


styleGroupContainer : List (Html.Attribute msg)
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


styleButtonInner : List (Html.Attribute msg)
styleButtonInner =
    [ class "ButtonInner"
    , css
        [ Css.displayFlex
        , Css.justifyContent Css.spaceBetween
        , Css.alignItems Css.center
        ]
    ]


styleIconContainer : List (Html.Attribute msg)
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


styleOuterContent : Bool -> MenuConfig msg -> Html.Attribute msg
styleOuterContent contentVisible config =
    css
        [ position absolute
        , zIndex (int <| config.zIndex + 1)
        , case config.alignment of
            Left ->
                left zero

            Right ->
                right zero
        ]


styleContent : Bool -> MenuConfig msg -> Html.Attribute msg
styleContent contentVisible config =
    css
        [ padding (px 25)
        , border3 (px 1) solid Colors.gray85
        , minWidth (px 202)
        , borderRadius (px 8)
        , backgroundColor Colors.white
        , listStyle Css.none
        , Shadows.high
        , before
            [ property "content" "\"\""
            , position absolute
            , top (px -2)
            , border3 (px 6) solid transparent
            , borderBottomColor Colors.gray85
            ]
        , after
            [ property "content" "\"\""
            , position absolute
            , top (px 1)
            , zIndex (int 2)
            , border3 (px 5) solid transparent
            , borderBottomColor Colors.white
            ]
        , case config.alignment of
            Left ->
                Css.batch
                    [ before [ left (px 19) ]
                    , after [ left (px 20) ]
                    ]

            Right ->
                Css.batch
                    [ before [ right (px 19) ]
                    , after [ right (px 20) ]
                    ]
        , if contentVisible then
            display block

          else
            display Css.none
        ]


styleContainer : List (Html.Attribute msg)
styleContainer =
    [ class "Container"
    , AttributesExtra.nriDescription "Nri-Ui-Menu-V3"
    , css
        [ position relative
        , display inlineBlock
        ]
    ]
