module Examples.Menu exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Role as Role
import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Examples.Button
import Examples.ClickableSvg
import Examples.ClickableText
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.Attributes.V2 exposing (safeIdWithPrefix)
import Nri.Ui.Menu.V4 as Menu
import Nri.Ui.RadioButton.V4 as RadioButton
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Switch.V3 as Switch
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.TextInput.V8 as TextInput
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import Routes
import Set exposing (Set)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttrs
import Task


moduleName : String
moduleName =
    "Menu"


version : Int
version =
    4


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Layout ]
    , keyboardSupport =
        [ { keys = [ Space ], result = "Opens the menu" }
        , { keys = [ Enter ], result = "Opens the menu" }
        , { keys = [ Tab ], result = "Takes focus out of the menu to the next focusable element & closes the menu." }
        , { keys = [ Tab, Shift ], result = "Takes focus out of the menu to the previous focusable element & closes the menu." }
        , { keys = [ Arrow KeyboardSupport.Up ]
          , result = "If menu is closed, opens the menu & selects the last menu item.\nIf menu is open, moves the focus to the previous menu item."
          }
        , { keys = [ Arrow KeyboardSupport.Down ]
          , result = "If menu is closed, opens the menu & selects the first menu item.\nIf menu is open, moves the focus to the next menu item."
          }
        , { keys = [ Esc ], result = "Closes the menu" }
        ]
    , preview = [ preview ]
    , about = []
    , view = view
    }


preview : Html Never
preview =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 100 100"
        , SvgAttrs.width "100%"
        , SvgAttrs.height "100%"
        , SvgAttrs.fill (ColorsExtra.toCssString Colors.white)
        , Role.img
        , SvgAttrs.filter "drop-shadow(0px 2px 2px rgb(0 0 0 / 0.4))"
        ]
        [ Svg.rect
            [ SvgAttrs.x "5"
            , SvgAttrs.y "10"
            , SvgAttrs.width "95"
            , SvgAttrs.height "60"
            , SvgAttrs.rx "5"
            ]
            []
        , Svg.polygon [ SvgAttrs.points "80,10 85,4 90,10" ] []
        , Svg.text_
            [ SvgAttrs.fill (ColorsExtra.toCssString Colors.azure)
            , SvgAttrs.css [ Fonts.baseFont, Css.fontSize (Css.px 6) ]
            , SvgAttrs.x "15"
            , SvgAttrs.y "28"
            ]
            [ Svg.text "Menu item 1" ]
        , Svg.text_
            [ SvgAttrs.fill (ColorsExtra.toCssString Colors.azure)
            , SvgAttrs.css [ Fonts.baseFont, Css.fontSize (Css.px 6) ]
            , SvgAttrs.x "15"
            , SvgAttrs.y "42"
            ]
            [ Svg.text "Menu item 2" ]
        , Svg.text_
            [ SvgAttrs.fill (ColorsExtra.toCssString Colors.azure)
            , SvgAttrs.css [ Fonts.baseFont, Css.fontSize (Css.px 6) ]
            , SvgAttrs.x "15"
            , SvgAttrs.y "56"
            ]
            [ Svg.text "Menu item 3" ]
        ]


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        menuAttributes =
            Control.currentValue state.settings
                |> .attributes
                |> List.map Tuple.second

        isOpen name =
            case state.openMenu of
                Just open ->
                    open == name

                Nothing ->
                    False
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControls
        , settings = state.settings
        , mainType = Just "RootHtml.Html Msg"
        , extraCode =
            [ "import Nri.Ui.Button.V10 as Button"
            , "import Nri.Ui.ClickableSvg.V2 as ClickableSvg"
            , "import Nri.Ui.ClickableText.V4 as ClickableText"
            , "import Nri.Ui.Tooltip.V3 as Tooltip"
            , Code.newlines
            , Code.unionType "Msg"
                [ "ToggleMenu { focus : Maybe String, isOpen : Bool }"
                , "ToggleTooltip Bool"
                ]
            ]
        , renderExample = Code.unstyledView
        , toExampleCode =
            \{ attributes } ->
                let
                    code : String
                    code =
                        Code.fromModule moduleName "view ToggleMenu"
                            ++ Code.listMultiline
                                ((if (Control.currentValue state.settings).withTooltip then
                                    [ Code.fromModule "Menu" "withTooltip"
                                        ++ Code.listMultiline
                                            [ Code.fromModule "Tooltip" "open " ++ (Code.bool <| Set.member "tooltip-0" state.openTooltips)
                                            , Code.fromModule "Tooltip" "onToggle " ++ "ToggleTooltip"
                                            , Code.fromModule "Tooltip" "plaintext " ++ Code.string "Tooltip content"
                                            , Code.fromModule "Tooltip" "fitToContent"
                                            ]
                                            2
                                    ]

                                  else
                                    []
                                 )
                                    ++ ("Menu.isOpen " ++ Code.bool (isOpen "interactiveExample"))
                                    :: List.map Tuple.first attributes
                                )
                                1
                            ++ Code.listMultiline
                                [ (Code.fromModule moduleName "entry " ++ Code.string "unique-button-id" ++ " <|")
                                    ++ Code.newlineWithIndent 2
                                    ++ Code.anonymousFunction "attributes"
                                        (Code.newlineWithIndent 2
                                            ++ (Code.fromModule "ClickableText" "button " ++ Code.string "Button")
                                            ++ Code.listMultiline
                                                [ Code.fromModule "ClickableText" "small"
                                                , Code.fromModule "ClickableText" "custom" ++ " attributes"
                                                ]
                                                3
                                        )
                                ]
                                1
                in
                [ { sectionName = "Example"
                  , code = code
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Interactive Example"
        , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
        ]
    , div [ css [ Css.displayFlex, Css.justifyContent Css.center ] ]
        [ Menu.view (FocusAndToggle "interactiveExample")
            ((if (Control.currentValue state.settings).withTooltip then
                [ Menu.withTooltip
                    [ Tooltip.open (Set.member "tooltip-0" state.openTooltips)
                    , Tooltip.onToggle (ToggleTooltip "tooltip-0")
                    , Tooltip.plaintext "Tooltip content"
                    , Tooltip.fitToContent
                    ]
                ]

              else
                []
             )
                ++ Menu.isOpen (isOpen "interactiveExample")
                :: menuAttributes
            )
            [ Menu.entry "customizable-example" <|
                \attrs ->
                    ClickableText.button "Button"
                        [ ClickableText.small
                        , ClickableText.onClick (ConsoleLog "Interactive example")
                        , ClickableText.custom attrs
                        ]
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Menu types"
        , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
        ]
    , Table.view []
        [ Table.string
            { header = "Menu type"
            , value = .menu
            , width = Css.pct 15
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = .example
            , width = Css.px 300
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Description"
            , view = \{ description } -> Text.smallBody [ Text.markdown description ]
            , width = Css.pct 60
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top ]
            , sort = Nothing
            }
        ]
        [ { menu = "Menu.navMenu (default)"
          , example =
                Menu.view (FocusAndToggle "1stPeriodEnglish")
                    (List.filterMap identity
                        [ Just <| Menu.buttonId "1stPeriodEnglish__button"
                        , Just <| Menu.menuId "1stPeriodEnglish__menu"
                        , Just <| Menu.defaultTrigger "1st Period English with Mx. Trainer" []
                        , Just <| Menu.isOpen (isOpen "1stPeriodEnglish")
                        ]
                    )
                    [ Menu.entry "hello-button" <|
                        \attrs ->
                            ClickableText.button "Hello"
                                [ ClickableText.onClick (ConsoleLog "Hello")
                                , ClickableText.small
                                , ClickableText.custom attrs
                                ]
                    , Menu.group "Menu group"
                        [ Menu.entry "gift-button" <|
                            \attrs ->
                                ClickableText.button "Gift"
                                    [ ClickableText.onClick (ConsoleLog "Gift")
                                    , ClickableText.small
                                    , ClickableText.custom attrs
                                    , ClickableText.icon UiIcon.gift
                                    ]
                        , Menu.entry "null-button" <|
                            \attrs ->
                                ClickableText.button "Nope!"
                                    [ ClickableText.onClick (ConsoleLog "Nope!")
                                    , ClickableText.small
                                    , ClickableText.custom attrs
                                    , ClickableText.icon UiIcon.null
                                    ]
                        , Menu.entry "no-icon-button" <|
                            \attrs ->
                                ClickableText.button "Skip"
                                    [ ClickableText.onClick (ConsoleLog "Skip")
                                    , ClickableText.small
                                    , ClickableText.custom attrs
                                    ]
                        ]
                    , Menu.entry "performance-button" <|
                        \attrs ->
                            ClickableText.button "Performance"
                                [ ClickableText.onClick (ConsoleLog "Performance")
                                , ClickableText.small
                                , ClickableText.custom attrs
                                ]
                    ]
          , description = "Makes the menu follow the [Navigation Menu pattern](https://www.w3.org/WAI/ARIA/apg/example-index/menu-button/menu-button-links.html), but without the ul/li structure."
          }
        , { menu = "Menu.navMenuList"
          , example =
                Menu.view (FocusAndToggle "dropdown_list")
                    [ Menu.buttonId "dropdown_list__button"
                    , Menu.menuId "dropdown_list__menu"
                    , Menu.navMenuList
                    , Menu.defaultTrigger "Dropdown list" []
                    , Menu.isOpen (isOpen "dropdown_list")
                    ]
                    [ Menu.entry "dropdown_list__first" <|
                        \attrs ->
                            ClickableText.button "First"
                                [ ClickableText.small
                                , ClickableText.onClick (ConsoleLog "First")
                                , ClickableText.custom attrs
                                ]
                    , Menu.entry "dropdown_list__second" <|
                        \attrs ->
                            ClickableText.button "Second"
                                [ ClickableText.small
                                , ClickableText.onClick (ConsoleLog "Second")
                                , ClickableText.custom attrs
                                ]
                    , Menu.entry "dropdown_list__third" <|
                        \attrs ->
                            ClickableText.button "Third"
                                [ ClickableText.small
                                , ClickableText.onClick (ConsoleLog "Third")
                                , ClickableText.custom attrs
                                ]
                    ]
          , description = """
Same as navMenu, except that a ul/li structure will be added as a fall-through.
    """
          }
        , { menu = "Menu.disclosure"
          , example =
                Menu.view (FocusAndToggle "with_disclosure")
                    [ Menu.buttonId "disclosure__button"
                    , Menu.menuId "disclosure__menu"
                    , Menu.disclosure { lastId = "disclosure__login__button" }
                    , Menu.defaultTrigger "Log In disclosure" []
                    , Menu.isOpen (isOpen "with_disclosure")
                    ]
                    [ Menu.entry "disclosure__username" <|
                        \attrs ->
                            div []
                                [ TextInput.view "Username"
                                    [ TextInput.id "disclosure__username__input"
                                    ]
                                , TextInput.view "Password"
                                    [ TextInput.id "disclosure__password__input"
                                    ]
                                , Button.button "Log in"
                                    [ Button.primary
                                    , Button.id "disclosure__login__button"
                                    , Button.fillContainerWidth
                                    , Button.css [ Css.marginTop (Css.px 15) ]
                                    ]
                                ]
                    ]
          , description =
                """
 Makes the menu behave as a disclosure.

For more information, please read [Disclosure (Show/Hide) pattern](https://www.w3.org/WAI/ARIA/apg/patterns/disclosure/).

You will need to pass in the last focusable element in the disclosed content in order for:

  - any focusable elements in the disclosed content to be keyboard accessible
  - the disclosure to close appropriately when the user tabs past all of the disclosed content
"""
          }
        , { menu = "Menu.dialog"
          , example =
                Menu.view (FocusAndToggle "dialog")
                    [ Menu.buttonId "dialog__button"
                    , Menu.menuId "dialog__menu"
                    , Menu.dialog { firstId = "dialog__username__input", lastId = "dialog__login__button" }
                    , Menu.defaultTrigger "Log In dialog" []
                    , Menu.isOpen (isOpen "dialog")
                    ]
                    [ Menu.entry "dialog__username" <|
                        \attrs ->
                            div []
                                [ TextInput.view "Username"
                                    [ TextInput.id "dialog__username__input"
                                    ]
                                , TextInput.view "Password"
                                    [ TextInput.id "dialog__password__input"
                                    ]
                                , Button.button "Log in"
                                    [ Button.primary
                                    , Button.id "dialog__login__button"
                                    , Button.fillContainerWidth
                                    , Button.css [ Css.marginTop (Css.px 15) ]
                                    ]
                                ]
                    ]
          , description =
                """
 Makes the menu behave as a dialog.

For more information, please read [Dialog pattern](https://w3c.github.io/aria-practices/examples/dialog-modal/dialog.html/).

You will need to pass in the first and last focusable element in the dialog content in order for:

  - any focusable elements in the dialog content to be keyboard accessible
  - the tab to wrap around appropriately when the user tabs past all of the dialog content

"""
          }
        ]
    , Heading.h2
        [ Heading.plaintext "Menu trigger base styles"
        , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
        ]
    , Table.view []
        [ Table.string
            { header = "Trigger type"
            , value = .menu
            , width = Css.px 0
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Type Signature"
            , view = \{ typeAnnotation } -> code [] [ text typeAnnotation ]
            , width = Css.pct 30
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.middle
                    , Css.fontSize (Css.px 12)
                    ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = .example
            , width = Css.pct 20
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Pattern Notes"
            , view = .patternNotes >> Text.smallBody
            , width = Css.pct 30
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
            , sort = Nothing
            }
        ]
        [ { menu = "Menu.defaultTrigger"
          , typeAnnotation = "String -> List (Button.Attribute msg) -> Attribute msg"
          , example =
                Menu.view (FocusAndToggle "defaultTrigger")
                    [ Menu.defaultTrigger "Log in" []
                    , Menu.isOpen (isOpen "defaultTrigger")
                    , Menu.buttonId "defaultTrigger"
                    , Menu.menuId "defaultTrigger"
                    ]
                    []
          , patternNotes =
                [ Text.html
                    [ text "Composes with "
                    , ClickableText.link "Button"
                        [ ClickableText.href (Routes.exampleHref Examples.Button.example)
                        , ClickableText.appearsInline
                        ]
                    , text ", so any attribute you use for a Button can also used with this trigger."
                    ]
                ]
          }
        , { menu = "Menu.button"
          , typeAnnotation = "String -> List (Button.Attribute msg) -> Menu.Attribute msg"
          , example =
                Menu.view (FocusAndToggle "button")
                    [ Menu.button "Log in" []
                    , Menu.isOpen (isOpen "button")
                    , Menu.buttonId "button"
                    , Menu.menuId "button"
                    ]
                    []
          , patternNotes =
                [ Text.html
                    [ text "Composes with "
                    , ClickableText.link "Button"
                        [ ClickableText.href (Routes.exampleHref Examples.Button.example)
                        , ClickableText.appearsInline
                        ]
                    , text ", so any attribute you use for a Button can also used with this trigger."
                    ]
                ]
          }
        , { menu = "Menu.clickableText"
          , typeAnnotation = "String -> List (ClickableText.Attribute msg) -> Menu.Attribute msg"
          , example =
                Menu.view (FocusAndToggle "clickableText")
                    [ Menu.clickableText "Log in" []
                    , Menu.isOpen (isOpen "clickableText")
                    , Menu.buttonId "clickableText"
                    , Menu.menuId "clickableText"
                    ]
                    []
          , patternNotes =
                [ Text.html
                    [ text "Composes with "
                    , ClickableText.link "ClickableText"
                        [ ClickableText.href (Routes.exampleHref Examples.ClickableText.example)
                        , ClickableText.appearsInline
                        ]
                    , text ", so any attribute you use for a ClickableText can also used with this trigger."
                    ]
                ]
          }
        , { menu = "Menu.clickableSvg with UiIcon.gear"
          , typeAnnotation = "String -> Svg.Svg -> List (ClickableSvg.Attribute msg) -> Menu.Attribute msg"
          , example =
                Menu.view (FocusAndToggle "clickableSvg")
                    [ Menu.clickableSvg "Log in" UiIcon.gear []
                    , Menu.isOpen (isOpen "clickableSvg")
                    , Menu.buttonId "clickableSvg"
                    , Menu.menuId "clickableSvg"
                    ]
                    []
          , patternNotes =
                [ Text.html
                    [ text "Composes with "
                    , ClickableText.link "ClickableSvg"
                        [ ClickableText.href (Routes.exampleHref Examples.ClickableSvg.example)
                        , ClickableText.appearsInline
                        ]
                    , text ", so any attribute you use for a ClickableSvg can also used with this trigger."
                    ]
                ]
          }
        , { menu = "Menu.clickableSvgWithoutIndicator with UiIcon.gear"
          , typeAnnotation = "String -> Svg.Svg -> List (ClickableSvg.Attribute msg) -> Menu.Attribute msg"
          , example =
                Menu.view (FocusAndToggle "clickableSvgWithoutIndicator")
                    [ Menu.clickableSvgWithoutIndicator "Log in" UiIcon.gear []
                    , Menu.isOpen (isOpen "clickableSvgWithoutIndicator")
                    , Menu.buttonId "clickableSvgWithoutIndicator"
                    , Menu.menuId "clickableSvgWithoutIndicator"
                    ]
                    []
          , patternNotes =
                [ Text.html
                    [ text "Composes with "
                    , ClickableText.link "ClickableSvg"
                        [ ClickableText.href (Routes.exampleHref Examples.ClickableSvg.example)
                        , ClickableText.appearsInline
                        ]
                    , text ", so any attribute you use for a ClickableSvg can also used with this trigger."
                    ]
                ]
          }
        ]
    , Heading.h2
        [ Heading.plaintext "Menu content"
        , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
        ]
    , Table.view []
        [ Table.string
            { header = "Content"
            , value = .name
            , width = Css.pct 20
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = \{ name, menuType, entries } -> forcedOpenExample name menuType entries
            , width = Css.pct 20
            , cellStyles =
                \{ name } ->
                    let
                        buttonId =
                            forcedOpenExampleButtonId name
                    in
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.middle
                    , Css.Global.descendants
                        [ -- when menus are open, we add this overlay to
                          -- ensure that clicks close the menu.
                          -- so if the menu is open, nothing else on the page
                          -- is interactive by mouse.
                          -- for the purposes of these static examples,
                          -- this is very unhelpful, so we hide the overlay.
                          --
                          -- if changing this code, please ensure the example in the CC is still interactive.
                          Css.Global.class "Nri-Menu-Overlay"
                            [ Css.display Css.none
                            ]
                        , -- Menus are absolutely positioned, but this is
                          -- pretty inconvenient for displaying them in a table.
                          -- This override is so that these Menu will be part of
                          -- the normal flow of the page, so that their
                          -- content is always visible.
                          Css.Global.selector ("#" ++ buttonId ++ " + div")
                            [ Css.position Css.relative ]
                        ]
                    ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Pattern Notes"
            , view = \{ about } -> Text.smallBody [ Text.markdown about ]
            , width = Css.pct 60
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 14) (Css.px 7)
                    , Css.verticalAlign Css.top
                    , Css.Global.descendants
                        [ Css.Global.pre [ Css.whiteSpace Css.preWrap ]
                        ]
                    ]
            , sort = Nothing
            }
        ]
        [ { name = "List of entries"
          , menuType = Menu.navMenuList
          , entries =
                [ Menu.entry "list-of-entries-clickable-text-entry" <|
                    \attributes ->
                        ClickableText.button "ClickableText"
                            [ ClickableText.small
                            , ClickableText.custom attributes
                            ]
                , Menu.entry "list-of-entries-button-entry" <|
                    \attributes ->
                        Button.button "Button"
                            [ Button.small
                            , Button.custom attributes
                            ]
                , Menu.entry "list-of-entries-clickablesvg-entry" <|
                    \attributes ->
                        ClickableSvg.button "ClickableSvg"
                            UiIcon.arrowPointingRightThick
                            [ ClickableSvg.small
                            , ClickableSvg.custom attributes
                            , ClickableSvg.withBorder
                            ]
                ]
          , about = "Pass any interactive elements in using `Menu.entry`."
          }
        , { name = "Grouped entries"
          , menuType = Menu.navMenuList
          , entries =
                [ List.range 1 2
                    |> List.map
                        (\i ->
                            Menu.entry ("group-clickable-text-entry-" ++ String.fromInt i) <|
                                \attributes ->
                                    ClickableText.button ("Thing " ++ String.fromInt i)
                                        [ ClickableText.small
                                        , ClickableText.custom attributes
                                        ]
                        )
                    |> Menu.group "Group of ClickableTexts"
                , List.range 1 3
                    |> List.map
                        (\i ->
                            Menu.entry ("group-button-entry-" ++ String.fromInt i) <|
                                \attributes ->
                                    Button.button ("Thing " ++ String.fromInt i)
                                        [ Button.small
                                        , Button.custom attributes
                                        ]
                        )
                    |> Menu.group "Group of Buttons"
                ]
          , about =
                """Use `Menu.group` to create named groups of entries.


The structures are recursive and flexible:

    group : String -> List (Entry msg) -> Entry msg

    entry : String -> (List (Html.Attribute msg) -> Html msg) -> Entry msg

"""
          }
        , { name = "Mix of singular entries and grouped entries"
          , menuType = Menu.disclosure { lastId = droppedStudentsId }
          , entries =
                [ Menu.entry "grades-and-perf" <|
                    \attributes ->
                        ClickableText.link "Grades and Performance Help"
                            [ ClickableText.linkExternal "https://noredink.zendesk.com/hc/en-us/sections/115001041146-Grades-Performance"
                            , ClickableText.custom attributes
                            , ClickableText.icon UiIcon.help
                            ]
                , Menu.group "Display scores as"
                    [ Menu.entry "percentages" <| viewScoreDisplay "Percentage" state.scoreDisplay
                    , Menu.entry "points" <| viewScoreDisplay "Points" state.scoreDisplay
                    ]
                , Menu.group "Dropped students"
                    [ Menu.entry "dropped-students" <| viewDroppedStudentsSwitch state.showDroppedStudents
                    ]
                ]
          , about =
                """
Because `group` and `entry` both result in the same type, individual entries and grouped entries can be displayed together.

Please note that depending on the interactive component you select, our composability may not work correctly yet.

In this realistic example, we can't actually pass the correct attributes to RadioButton or Switch because it will cause the events for those components to be swallowed, rendering them inoperable.
"""
          }
        ]
    ]


forcedOpenExample : String -> Menu.Attribute Msg -> List (Menu.Entry Msg) -> Html Msg
forcedOpenExample name type_ =
    Menu.view (FocusAndToggle name)
        [ Menu.clickableSvgWithoutIndicator (name ++ " example")
            UiIcon.arrowDown
            [ ClickableSvg.exactSize 15
            , ClickableSvg.css [ Css.marginLeft (Css.px 17) ]
            ]
        , Menu.isOpen True
        , Menu.buttonId (forcedOpenExampleButtonId name)
        , Menu.menuId (safeIdWithPrefix name "menuId")
        , Menu.alignLeft
        , type_
        ]


forcedOpenExampleButtonId : String -> String
forcedOpenExampleButtonId name =
    safeIdWithPrefix name "buttonId"


viewScoreDisplay : String -> Maybe String -> List (Attribute Msg) -> Html Msg
viewScoreDisplay value selected attributes =
    RadioButton.view
        { label = value
        , value = value
        , name = "score-display"
        , selectedValue = selected
        , valueToString = identity
        }
        [ -- To Fix: when the Menu attributes are attached to the RadioButton
          -- (as is required for the focus trap to work correctly),
          -- the RadioButtons become inoperable.
          -- RadioButton.custom attributes ,
          RadioButton.onSelect SelectScoreDisplay
        ]


viewDroppedStudentsSwitch : Bool -> List (Attribute Msg) -> Html Msg
viewDroppedStudentsSwitch showDroppedStudents attributes =
    Switch.view
        { label = "Show dropped students"
        , id = droppedStudentsId
        }
        [ Switch.onSwitch ShowDroppedStudents
        , Switch.selected showDroppedStudents

        -- To Fix: when the Menu attributes are attached to the Switch
        -- (as is required for the focus trap to work correctly),
        -- the Switch become inoperable.
        --, Switch.custom attributes
        ]


droppedStudentsId : String
droppedStudentsId =
    "show-dropped-students"


{-| -}
init : State
init =
    { openMenu = Nothing
    , checkboxChecked = False
    , openTooltips = Set.empty
    , settings = initSettings
    , scoreDisplay = Nothing
    , showDroppedStudents = False
    }


{-| -}
type alias State =
    { openMenu : Maybe Id
    , checkboxChecked : Bool
    , openTooltips : Set String
    , settings : Control Settings
    , scoreDisplay : Maybe String
    , showDroppedStudents : Bool
    }


type alias Settings =
    { attributes : List ( String, Menu.Attribute Msg )
    , withTooltip : Bool
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "" initSettingAttributes
        |> Control.field "withTooltip" (Control.bool False)


initSettingAttributes : Control (List ( String, Menu.Attribute Msg ))
initSettingAttributes =
    Control.list
        |> ControlExtra.listItems "popout"
            (Control.list
                |> ControlExtra.optionalListItem "alignment" controlAlignment
                |> ControlExtra.optionalListItem "menuWidth" controlMenuWidth
            )
        |> ControlExtra.listItems "triggering element"
            (Control.list
                |> ControlExtra.optionalBoolListItem "isDisabled" ( "Menu.isDisabled True", Menu.isDisabled True )
                |> ControlExtra.optionalBoolListItem "opensOnHover" ( "Menu.opensOnHover True", Menu.opensOnHover True )
                |> ControlExtra.listItem "trigger" controlTrigger
            )


controlAlignment : Control ( String, Menu.Attribute msg )
controlAlignment =
    CommonControls.choice moduleName
        [ ( "alignLeft", Menu.alignLeft ), ( "alignRight", Menu.alignRight ) ]


controlTrigger : Control ( String, Menu.Attribute msg )
controlTrigger =
    Control.choice
        [ ( "defaultTrigger"
          , Control.map
                (\( c, a ) ->
                    ( "Menu.defaultTrigger " ++ Code.string "Menu" ++ " " ++ Code.list c
                    , Menu.defaultTrigger "Menu" a
                    )
                )
                controlButtonAttributes
          )
        , ( "button"
          , Control.map
                (\( c, a ) ->
                    ( "Menu.button " ++ Code.string "Menu" ++ " " ++ Code.list c
                    , Menu.button "Menu" a
                    )
                )
                controlButtonAttributes
          )
        , ( "clickableText"
          , Control.map
                (\( c, a ) ->
                    ( "Menu.clickableText " ++ Code.string "Menu" ++ " " ++ Code.list c
                    , Menu.clickableText "Menu" a
                    )
                )
                controlClickableTextAttributes
          )
        , ( "clickableSvg"
          , Control.map
                (\( ( iconStr, icon ), ( withBorderStr, withBorder ) ) ->
                    ( "Menu.clickableSvg "
                        ++ Code.string "Menu"
                        ++ " "
                        ++ iconStr
                        ++ " "
                        ++ Code.list
                            (if withBorder then
                                [ "ClickableSvg.withBorder", "ClickableSvg.exactWidth 55" ]

                             else
                                []
                            )
                    , Menu.clickableSvg "Menu"
                        icon
                        (if withBorder then
                            [ ClickableSvg.withBorder, ClickableSvg.exactWidth 55 ]

                         else
                            []
                        )
                    )
                )
                (Control.record (\a b -> ( a, b ))
                    |> Control.field "icon" (CommonControls.rotatedUiIcon 1)
                    |> Control.field "withBorder" (ControlExtra.bool True)
                )
          )
        , ( "clickableSvgWithoutIndicator"
          , Control.map
                (\( ( iconStr, icon ), ( withBorderStr, withBorder ) ) ->
                    ( "Menu.clickableSvgWithoutIndicator "
                        ++ Code.string "Menu"
                        ++ " "
                        ++ iconStr
                        ++ " "
                        ++ Code.list
                            (if withBorder then
                                [ "ClickableSvg.withBorder", "ClickableSvg.exactWidth 55" ]

                             else
                                []
                            )
                    , Menu.clickableSvgWithoutIndicator "Menu"
                        icon
                        (if withBorder then
                            [ ClickableSvg.withBorder, ClickableSvg.exactWidth 55 ]

                         else
                            []
                        )
                    )
                )
                (Control.record (\a b -> ( a, b ))
                    |> Control.field "icon" (CommonControls.rotatedUiIcon 1)
                    |> Control.field "withBorder" (ControlExtra.bool True)
                )
          )
        ]


controlButtonAttributes : Control ( List String, List (Button.Attribute msg) )
controlButtonAttributes =
    Control.list
        |> CommonControls.iconNotCheckedByDefault "Button" Button.icon
        |> ControlExtra.optionalListItem "exactWidth"
            (Control.map
                (\i ->
                    ( Code.fromModule "Button" "exactWidth " ++ String.fromInt i
                    , Button.exactWidth i
                    )
                )
                (Control.int 220)
            )
        |> Control.map List.unzip


controlClickableTextAttributes : Control ( List String, List (ClickableText.Attribute msg) )
controlClickableTextAttributes =
    Control.list
        |> CommonControls.iconNotCheckedByDefault "ClickableText" ClickableText.icon
        |> Control.map List.unzip


controlMenuWidth : Control ( String, Menu.Attribute msg )
controlMenuWidth =
    Control.map
        (\val -> ( "Menu.menuWidth " ++ String.fromInt val, Menu.menuWidth val ))
        (Control.int 220)


{-| -}
type Msg
    = ConsoleLog String
    | UpdateControls (Control Settings)
    | ToggleTooltip String Bool
    | FocusAndToggle String { isOpen : Bool, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | SelectScoreDisplay String
    | ShowDroppedStudents Bool


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            ( Debug.log "Menu Example" message |> always state, Cmd.none )

        UpdateControls configuration ->
            ( { state | settings = configuration }, Cmd.none )

        ToggleTooltip id True ->
            ( { state | openTooltips = Set.insert id state.openTooltips }, Cmd.none )

        ToggleTooltip id False ->
            ( { state | openTooltips = Set.remove id state.openTooltips }, Cmd.none )

        FocusAndToggle id { isOpen, focus } ->
            ( { state
                | openMenu =
                    if isOpen then
                        Just id

                    else
                        Nothing
              }
            , Maybe.map (\idString -> Task.attempt Focused (Dom.focus idString)) focus
                |> Maybe.withDefault Cmd.none
            )

        Focused _ ->
            ( state, Cmd.none )

        SelectScoreDisplay name ->
            ( { state | scoreDisplay = Just name }, Cmd.none )

        ShowDroppedStudents showDroppedStudents ->
            ( { state | showDroppedStudents = showDroppedStudents }, Cmd.none )



-- INTERNAL


type alias Id =
    String
