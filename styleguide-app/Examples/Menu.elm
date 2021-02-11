module Examples.Menu exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (..)
import AtomicDesignType exposing (AtomicDesignType(..))
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Menu.V2 as Menu
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import Set exposing (Set)
import Task


{-| -}
example : Example State Msg
example =
    { name = "Menu"
    , version = 2
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Widgets ]
    , atomicDesignType = Molecule
    , keyboardSupport =
        [ { keys = [ Space ], result = "Opens the menu" }
        , { keys = [ Enter ], result = "Opens the menu" }
        , { keys = [ Tab ], result = "Takes focus out of the menu to the next focusable element." }
        , { keys = [ Tab, Shift ], result = "Takes focus out of the menu to the previous focusable element." }
        , { keys = [ Arrow KeyboardSupport.Up ]
          , result = "While menu is open, moves the focus to the previous menu item button (wraps focus to the last menu item)"
          }
        , { keys = [ Arrow KeyboardSupport.Down ]
          , result = "While menu is closed, moves the focus to the next menu item button (wraps focus to the first menu item)"
          }
        , { keys = [ Esc ], result = "Closes the menu" }
        ]
    , view = view
    }


view : State -> List (Html Msg)
view state =
    let
        viewConfiguration =
            Control.currentValue state.viewConfiguration

        iconButtonWithMenuConfiguration =
            Control.currentValue state.iconButtonWithMenuConfiguration

        isOpen name =
            case state.openMenu of
                Just open ->
                    open == name

                Nothing ->
                    False
    in
    [ div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
        [ Html.h3 [ css [ Css.width (Css.pct 100) ] ] [ Html.text "Nri.Menu.view" ]
        , viewControl SetViewConfiguration state.viewConfiguration
        , Menu.view
            { isOpen = isOpen "1stPeriodEnglish"
            , toggle = menuToggler "1stPeriodEnglish"
            , focus = Focus
            , buttonId = "1stPeriodEnglish__button"
            , menuId = "1stPeriodEnglish__menu"
            , title = "1st Period English with Mx. Trainer"
            , icon = viewConfiguration.icon
            , hasBorder = viewConfiguration.hasBorder
            , alignment = viewConfiguration.alignment
            , wrapping = viewConfiguration.wrapping
            , isDisabled = viewConfiguration.isDisabled
            , buttonWidth = viewConfiguration.buttonWidth
            , menuWidth = viewConfiguration.menuWidth
            , entries =
                [ Menu.group "Buttons"
                    [ Menu.entry <|
                        ClickableText.button "Hello"
                            [ ClickableText.onClick (ConsoleLog "Hello")
                            , ClickableText.small
                            ]
                    , Menu.entry <|
                        ClickableText.button "Performance"
                            [ ClickableText.onClick (ConsoleLog "Performance")
                            , ClickableText.small
                            ]
                    ]
                ]
            }
        ]
    , div
        [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
        [ Html.h3 [ css [ Css.width (Css.pct 100) ] ] [ Html.text "Nri.Menu.iconButtonWithMenu" ]
        , viewControl SetIconButtonWithMenuConfiguration state.iconButtonWithMenuConfiguration
        , Menu.iconButtonWithMenu
            { isTooltipOpen = Set.member "iconButtonWithMenu" state.openTooltips
            , onShowTooltip = ShowTooltip "iconButtonWithMenu"
            , buttonId = "icon-button-with-menu__button"
            , menuId = "icon-button-with-menu__menu"
            , label = "Menu.iconButtonWithMenu: Click me!"
            , isOpen = isOpen "icon-button-with-menu"
            , toggle = menuToggler "icon-button-with-menu"
            , focus = Focus
            , icon = iconButtonWithMenuConfiguration.icon
            , alignment = iconButtonWithMenuConfiguration.alignment
            , isDisabled = iconButtonWithMenuConfiguration.isDisabled
            , menuWidth = iconButtonWithMenuConfiguration.menuWidth
            , entries = []
            }
        ]
    ]


viewControl : (Control a -> Msg) -> Control a -> Html Msg
viewControl setControl control =
    code
        [ css [ Css.minWidth (Css.px 300), Css.marginRight (Css.px 20) ] ]
        [ Control.view setControl control
            |> fromUnstyled
        ]


{-| -}
init : State
init =
    { openMenu = Nothing
    , checkboxChecked = False
    , openTooltips = Set.empty
    , viewConfiguration = initViewConfiguration
    , iconButtonWithMenuConfiguration = initIconButtonWithMenuConfiguration
    }


{-| -}
type alias State =
    { openMenu : Maybe Id
    , checkboxChecked : Bool
    , openTooltips : Set String
    , viewConfiguration : Control ViewConfiguration
    , iconButtonWithMenuConfiguration : Control IconButtonWithMenuConfiguration
    }


type alias ViewConfiguration =
    { isDisabled : Bool
    , hasBorder : Bool
    , alignment : Menu.Alignment
    , wrapping : Menu.TitleWrapping
    , buttonWidth : Maybe Int
    , menuWidth : Maybe Int
    , icon : Maybe Svg
    }


initViewConfiguration : Control ViewConfiguration
initViewConfiguration =
    Control.record ViewConfiguration
        |> Control.field "isDisabled" (Control.bool False)
        |> Control.field "hasBorder" (Control.bool True)
        |> Control.field "alignment"
            (Control.choice
                [ ( "Right", Control.value Menu.Right )
                , ( "Left", Control.value Menu.Left )
                ]
            )
        |> Control.field "wrapping"
            (Control.choice
                [ ( "WrapAndExpandTitle", Control.value Menu.WrapAndExpandTitle )
                , ( "TruncateTitle", Control.value Menu.TruncateTitle )
                ]
            )
        |> Control.field "buttonWidth"
            (Control.maybe False (Control.choice [ ( "220", Control.value 220 ) ]))
        |> Control.field "menuWidth"
            (Control.maybe False (Control.choice [ ( "180", Control.value 220 ) ]))
        |> Control.field "icon"
            (Control.maybe False
                (Control.choice
                    [ ( "gift", Control.value UiIcon.gift )
                    , ( "hat", Control.value UiIcon.hat )
                    , ( "star", Control.value UiIcon.star )
                    ]
                )
            )


type alias IconButtonWithMenuConfiguration =
    { isDisabled : Bool
    , alignment : Menu.Alignment
    , menuWidth : Maybe Int
    , icon : Svg
    }


initIconButtonWithMenuConfiguration : Control IconButtonWithMenuConfiguration
initIconButtonWithMenuConfiguration =
    Control.record IconButtonWithMenuConfiguration
        |> Control.field "isDisabled" (Control.bool False)
        |> Control.field "alignment"
            (Control.choice
                [ ( "Left", Control.value Menu.Left )
                , ( "Right", Control.value Menu.Right )
                ]
            )
        |> Control.field "menuWidth"
            (Control.maybe False (Control.choice [ ( "180", Control.value 220 ) ]))
        |> Control.field "icon"
            (Control.choice
                [ ( "edit", Control.value UiIcon.edit )
                , ( "share", Control.value UiIcon.share )
                , ( "gear", Control.value UiIcon.gear )
                ]
            )


{-| -}
type Msg
    = SetMenu (Maybe Id)
    | ShowTooltip String Bool
    | ConsoleLog String
    | SetViewConfiguration (Control ViewConfiguration)
    | SetIconButtonWithMenuConfiguration (Control IconButtonWithMenuConfiguration)
    | Focus String
    | Focused (Result Dom.Error ())
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetMenu maybeId ->
            ( { state | openMenu = maybeId }, Cmd.none )

        ShowTooltip key isOpen ->
            ( { state
                | openTooltips =
                    if isOpen then
                        Set.insert key state.openTooltips

                    else
                        Set.remove key state.openTooltips
              }
            , Cmd.none
            )

        ConsoleLog message ->
            let
                _ =
                    Debug.log "Menu Example" message
            in
            ( state, Cmd.none )

        SetViewConfiguration configuration ->
            ( { state | viewConfiguration = configuration }, Cmd.none )

        SetIconButtonWithMenuConfiguration configuration ->
            ( { state | iconButtonWithMenuConfiguration = configuration }, Cmd.none )

        Focus idString ->
            ( state, Task.attempt Focused (Dom.focus idString) )

        Focused _ ->
            ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Id =
    String


type alias Value =
    String


menuToggler : Id -> Bool -> Msg
menuToggler id desiresToBeOpen =
    if desiresToBeOpen then
        SetMenu (Just id)

    else
        SetMenu Nothing
