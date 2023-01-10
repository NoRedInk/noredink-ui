module Nri.Ui.Menu.V4 exposing
    ( view, button, custom, Config
    , Attribute, Button, ButtonAttribute
    , navMenuList, disclosure, dialog
    , isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover
    , Alignment(..), alignment
    , icon, wrapping, hasBorder, buttonWidth
    , TitleWrapping(..)
    , Entry, group, entry
    )

{-| Changes from V3:

  - improve composability with Button, ClickableText, and ClickableSvg

A togglable menu view and related buttons.

<https://zpl.io/a75OrE2>


## Menu rendering

@docs view, button, custom, Config
@docs Attribute, Button, ButtonAttribute


## Menu attributes

@docs navMenuList, disclosure, dialog
@docs isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover
@docs Alignment, alignment


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
    = Attribute (MenuConfig -> MenuConfig)


{-| -}
type Button msg
    = StandardButton (MenuConfig -> List (Html.Attribute msg) -> Html msg)
    | CustomButton (List (Html.Attribute msg) -> Html msg)


{-| -}
type alias Config msg =
    { button : Button msg
    , entries : List (Entry msg)
    , isOpen : Bool
    , focusAndToggle : { isOpen : Bool, focus : Maybe String } -> msg
    }


type alias MenuConfig =
    { alignment : Alignment
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
    = NavMenuList
    | NavMenu
    | Disclosure { lastId : String }
    | Dialog
        { firstId : String
        , lastId : String
        }



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


{-| Makes the menu follow the [Navigation Menu pattern](https://www.w3.org/WAI/ARIA/apg/example-index/menu-button/menu-button-links.html).
-}
navMenuList : Attribute msg
navMenuList =
    Attribute <| \config -> { config | purpose = NavMenuList }


{-| Makes the menu behave as a disclosure.

For more information, please read [Disclosure (Show/Hide) pattern](https://www.w3.org/WAI/ARIA/apg/patterns/disclosure/).

You will need to pass in the last focusable element in the disclosed content in order for:

  - any focusable elements in the disclosed content to be keyboard accessible
  - the disclosure to close appropriately when the user tabs past all of the disclosed content

-}
disclosure : { lastId : String } -> Attribute msg
disclosure exitFocusManager =
    Attribute (\config -> { config | purpose = Disclosure exitFocusManager })


{-| Makes the menu behave as a dialog.

For more information, please read [Dialog pattern](https://w3c.github.io/aria-practices/examples/dialog-modal/dialog.html/).

You will need to pass in the first and last focusable element in the dialog content in order for:

  - any focusable elements in the dialog content to be keyboard accessible
  - the tab to wrap around appropriately when the user tabs past all of the dialog content

-}
dialog : { firstId : String, lastId : String } -> Attribute msg
dialog exitFocusManager =
    Attribute (\config -> { config | purpose = Dialog exitFocusManager })


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
        menuConfig =
            List.foldl (\(Attribute attr) c -> attr c)
                { alignment = Right
                , isDisabled = False
                , menuWidth = Nothing
                , buttonId = ""
                , menuId = ""
                , zIndex = 1
                , opensOnHover = False
                , purpose = NavMenu
                }
                attributes
    in
    viewCustom config menuConfig



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
                        [ Css.outline3 (Css.px 2) Css.solid Css.transparent
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
                                backgroundColor Colors.gray92

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
                    [ viewTitle title buttonConfig menuConfig

                    --, -- TODO: re-add arrow
                    --viewArrow menuConfig
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
    String
    ->
        { buttonConfig
            | icon : Maybe Svg.Svg
            , wrapping : TitleWrapping
        }
    -> { menuConfig | isDisabled : Bool }
    -> Html msg
viewTitle title_ config menuConfig =
    div styleTitle
        [ viewJust (\iconSvg -> span (styleIconContainer menuConfig) [ Svg.toHtml iconSvg ])
            config.icon
        , span
            (case config.wrapping of
                WrapAndExpandTitle ->
                    [ Attributes.attribute "data-nri-description" title_ ]

                TruncateTitle ->
                    [ class "Truncated"
                    , css
                        [ whiteSpace noWrap
                        , overflow hidden
                        , textOverflow ellipsis
                        ]
                    ]
            )
            [ Html.text title_ ]
        ]


viewCustom : Config msg -> MenuConfig -> Html msg
viewCustom config1 config =
    let
        ( maybeFirstFocusableElementId, maybeLastFocusableElementId ) =
            ( List.head (getFirstIds config1.entries), List.head (getLastIds config1.entries) )

        contentVisible =
            config1.isOpen && not config.isDisabled

        navMenuEvents =
            Key.onKeyDown
                [ Key.escape
                    (config1.focusAndToggle
                        { isOpen = False
                        , focus = Just config.buttonId
                        }
                    )
                , Key.tab
                    (config1.focusAndToggle
                        { isOpen = False
                        , focus = Nothing
                        }
                    )
                , Key.tabBack
                    (config1.focusAndToggle
                        { isOpen = False
                        , focus = Nothing
                        }
                    )
                ]

        entriesContainer attributes =
            case config.purpose of
                NavMenuList ->
                    ul (Role.menu :: attributes)

                NavMenu ->
                    div (Role.menu :: attributes)

                Disclosure _ ->
                    div attributes

                Dialog _ ->
                    div (Role.dialog :: attributes)
    in
    div
        (Attributes.id (config.buttonId ++ "__container")
            :: (case config.purpose of
                    NavMenuList ->
                        navMenuEvents

                    NavMenu ->
                        navMenuEvents

                    Disclosure { lastId } ->
                        WhenFocusLeaves.onKeyDown
                            [ Key.escape
                                (config1.focusAndToggle
                                    { isOpen = False
                                    , focus = Just config.buttonId
                                    }
                                )
                            ]
                            { firstId = config.buttonId
                            , lastId = lastId
                            , tabBackAction =
                                config1.focusAndToggle
                                    { isOpen = False
                                    , focus = Nothing
                                    }
                            , tabForwardAction =
                                config1.focusAndToggle
                                    { isOpen = False
                                    , focus = Nothing
                                    }
                            }

                    Dialog { firstId, lastId } ->
                        WhenFocusLeaves.onKeyDownPreventDefault
                            [ Key.escape
                                (config1.focusAndToggle
                                    { isOpen = False
                                    , focus = Just config.buttonId
                                    }
                                )
                            ]
                            { firstId = firstId
                            , lastId = lastId
                            , tabBackAction =
                                config1.focusAndToggle
                                    { isOpen = True
                                    , focus = Just lastId
                                    }
                            , tabForwardAction =
                                config1.focusAndToggle
                                    { isOpen = True
                                    , focus = Just firstId
                                    }
                            }
               )
            :: styleContainer
        )
        [ if config1.isOpen then
            div
                (Events.onClick
                    (config1.focusAndToggle
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
                , if config1.isOpen then
                    zIndex (int <| config.zIndex + 1)

                  else
                    Css.batch []
                ]
            , if not config.isDisabled && config.opensOnHover && config1.isOpen then
                Events.onMouseLeave
                    (config1.focusAndToggle
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
                    , -- Whether the menu is open or closed, move to the
                      -- first menu item if the "down" arrow is pressed
                      -- as long as it's not a Disclosure or Dialog
                      case ( config.purpose == NavMenuList || config.purpose == NavMenu, maybeFirstFocusableElementId, maybeLastFocusableElementId ) of
                        ( True, Just firstFocusableElementId, Just lastFocusableElementId ) ->
                            Key.onKeyDownPreventDefault
                                [ Key.down
                                    (config1.focusAndToggle
                                        { isOpen = True
                                        , focus = Just firstFocusableElementId
                                        }
                                    )
                                , Key.up
                                    (config1.focusAndToggle
                                        { isOpen = True
                                        , focus = Just lastFocusableElementId
                                        }
                                    )
                                ]

                        _ ->
                            AttributesExtra.none
                    , Attributes.id config.buttonId
                    , if config.isDisabled then
                        AttributesExtra.none

                      else
                        Events.custom "click"
                            (Json.Decode.succeed
                                { preventDefault = True
                                , stopPropagation = True
                                , message =
                                    case ( config1.isOpen, config.purpose ) of
                                        ( False, Dialog { firstId } ) ->
                                            config1.focusAndToggle
                                                { isOpen = True
                                                , focus = Just firstId
                                                }

                                        _ ->
                                            config1.focusAndToggle
                                                { isOpen = not config1.isOpen
                                                , focus = Nothing
                                                }
                                }
                            )
                    , if not config.isDisabled && config.opensOnHover then
                        Events.onMouseEnter
                            (config1.focusAndToggle
                                { isOpen = True
                                , focus = Nothing
                                }
                            )

                      else
                        AttributesExtra.none
                    ]
                        ++ (case config.purpose of
                                NavMenuList ->
                                    [ Aria.hasMenuPopUp
                                    , Aria.expanded config1.isOpen
                                    , Aria.controls [ config.menuId ]
                                    ]

                                NavMenu ->
                                    [ Aria.hasMenuPopUp
                                    , Aria.expanded config1.isOpen
                                    , Aria.controls [ config.menuId ]
                                    ]

                                Disclosure _ ->
                                    [ Aria.expanded config1.isOpen
                                    , Aria.controls [ config.menuId ]
                                    ]

                                Dialog _ ->
                                    [ Aria.hasDialogPopUp ]
                           )
              in
              case config1.button of
                StandardButton standardButton ->
                    standardButton config buttonAttributes

                CustomButton customButton ->
                    customButton buttonAttributes
            , div [ styleOuterContent contentVisible config ]
                [ div
                    [ AttributesExtra.nriDescription "menu-hover-bridge"
                    , css [ Css.height (px 10) ]
                    ]
                    []
                , entriesContainer
                    [ classList [ ( "Content", True ), ( "ContentVisible", contentVisible ) ]
                    , styleContent contentVisible config
                    , Aria.labelledBy config.buttonId
                    , Attributes.id config.menuId
                    , css
                        [ Maybe.map (\w -> Css.width (Css.px (toFloat w))) config.menuWidth
                            |> Maybe.withDefault (Css.batch [])
                        ]
                    ]
                    (viewEntries config
                        { focusAndToggle = config1.focusAndToggle
                        , previousId = Maybe.withDefault "" maybeLastFocusableElementId
                        , nextId = Maybe.withDefault "" maybeFirstFocusableElementId
                        }
                        config1.entries
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
    MenuConfig
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
    MenuConfig
    -> ({ isOpen : Bool, focus : Maybe String } -> msg)
    -> { upId : String, downId : String, entry_ : Entry msg }
    -> Html msg
viewEntry config focusAndToggle { upId, downId, entry_ } =
    case entry_ of
        Single id view_ ->
            let
                entryContainer attributes =
                    if config.purpose == NavMenuList then
                        li (Attributes.attribute "role" "none" :: attributes)

                    else
                        div attributes
            in
            entryContainer
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
                    , Key.onKeyDownPreventDefault
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


styleOverlay : MenuConfig -> List (Html.Attribute msg)
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


styleGroupTitleText : MenuConfig -> List (Html.Attribute msg)
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


styleIconContainer : { menuConfig | isDisabled : Bool } -> List (Html.Attribute msg)
styleIconContainer config =
    [ class "IconContainer"
    , css
        [ width (px 21)
        , height (px 21)
        , marginRight (px 5)
        , display inlineBlock
        , Css.flexShrink (Css.num 0)
        , color
            (if config.isDisabled then
                Colors.gray20

             else
                Colors.azure
            )
        ]
    ]


styleOuterContent : Bool -> MenuConfig -> Html.Attribute msg
styleOuterContent _ config =
    css
        [ position absolute
        , zIndex (int <| config.zIndex + 1)
        , case config.alignment of
            Left ->
                left zero

            Right ->
                right zero
        ]


styleContent : Bool -> MenuConfig -> Html.Attribute msg
styleContent contentVisible config =
    css
        [ padding (px 25)
        , margin zero
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
