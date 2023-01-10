module Nri.Ui.Menu.V4 exposing
    ( view, Config
    , Attribute
    , Button
    , button, clickableText, custom
    , navMenuList, disclosure, dialog
    , isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover
    , Alignment(..), alignment
    , Entry, group, entry
    )

{-| Changes from V3:

  - improve composability with Button, ClickableText, and ClickableSvg

A togglable menu view and related buttons.

<https://zpl.io/a75OrE2>


## Menu rendering

@docs view, Config
@docs Attribute


## Triggering button options

@docs Button
@docs button, clickableText, custom


## Menu attributes

@docs navMenuList, disclosure, dialog
@docs isDisabled, menuWidth, buttonId, menuId, menuZIndex, opensOnHover
@docs Alignment, alignment


## Menu content

@docs Entry, group, entry

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (class, classList, css)
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.AnimatedIcon.V1 as AnimatedIcon
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.WhenFocusLeaves.V1 as WhenFocusLeaves


{-| -}
type Attribute msg
    = Attribute (MenuConfig -> MenuConfig)


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


type Purpose
    = NavMenuList
    | NavMenu
    | Disclosure { lastId : String }
    | Dialog
        { firstId : String
        , lastId : String
        }



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


{-| Whether the menu content sticks to the left or right side of the button
-}
type Alignment
    = Left
    | Right



-- Triggering Button


{-| -}
type Button msg
    = StandardButton (MenuConfig -> Bool -> List (Html.Attribute msg) -> Html msg)
    | ClickableText (MenuConfig -> Bool -> List (Html.Attribute msg) -> Html msg)
    | CustomButton (List (Html.Attribute msg) -> Html msg)


{-| Defines a standard `Button` for the Menu.
-}
button : String -> List (Button.Attribute msg) -> Button msg
button title attributes =
    let
        disclosureIndicator : Bool -> Svg.Svg
        disclosureIndicator isOpen =
            Svg.withColor Colors.azure (AnimatedIcon.arrowDownUp isOpen)
    in
    StandardButton
        (\menuConfig isOpen buttonAttributes ->
            Button.button title
                ([ Button.tertiary
                 , Button.css
                    [ Css.justifyContent Css.spaceBetween
                    , Css.paddingLeft (Css.px 15)
                    , Css.paddingRight (Css.px 15)
                    , Css.color Colors.gray20 |> Css.important
                    , Css.fontWeight (Css.int 600)
                    , Css.hover [ Css.backgroundColor Colors.white ]
                    ]
                 , Button.custom buttonAttributes
                 , if menuConfig.isDisabled then
                    Button.disabled

                   else
                    Button.css []
                 , Button.rightIcon (disclosureIndicator isOpen)
                 ]
                    ++ attributes
                )
        )


{-| Use ClickableText as the triggering element for the Menu.
-}
clickableText : String -> List (ClickableText.Attribute msg) -> Button msg
clickableText title additionalAttributes =
    ClickableText
        (\menuConfig isOpen buttonAttributes ->
            ClickableText.button title
                ([ ClickableText.small
                 , ClickableText.custom buttonAttributes
                 , ClickableText.disabled menuConfig.isDisabled
                 , ClickableText.rightIcon (AnimatedIcon.arrowDownUp isOpen)
                 ]
                    ++ additionalAttributes
                )
        )


{-| Defines a custom `Button` for the menu
-}
custom : (List (Html.Attribute msg) -> Html msg) -> Button msg
custom builder =
    CustomButton builder


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
                    standardButton config config1.isOpen buttonAttributes

                ClickableText rendreButton ->
                    rendreButton config config1.isOpen buttonAttributes

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
