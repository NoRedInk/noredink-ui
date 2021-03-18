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
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Menu.V2 as Menu
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V2 as Tooltip
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
    , view = view
    }


view : State -> List (Html Msg)
view state =
    let
        viewConfiguration =
            Control.currentValue state.viewConfiguration

        viewCustomConfiguration =
            Control.currentValue state.viewCustomConfiguration

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
            , focusAndToggle = FocusAndToggle "1stPeriodEnglish"
            , buttonId = "1stPeriodEnglish__button"
            , menuId = "1stPeriodEnglish__menu"
            , alignment = viewConfiguration.alignment
            , isDisabled = viewConfiguration.isDisabled
            , menuWidth = viewConfiguration.menuWidth
            , entries =
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
            }
            { title = "1st Period English with Mx. Trainer"
            , icon = viewConfiguration.icon
            , hasBorder = viewConfiguration.hasBorder
            , wrapping = viewConfiguration.wrapping
            , buttonWidth = viewConfiguration.buttonWidth
            }
        ]
    , div
        [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
        [ Html.h3 [ css [ Css.width (Css.pct 100) ] ] [ Html.text "Nri.Menu.viewCustom" ]
        , viewControl SetIconButtonWithMenuConfiguration state.viewCustomConfiguration
        , Menu.viewCustom
            { buttonId = "icon-button-with-menu__button"
            , menuId = "icon-button-with-menu__menu"
            , isOpen = isOpen "icon-button-with-menu"
            , focusAndToggle = FocusAndToggle "icon-button-with-menu"
            , alignment = viewCustomConfiguration.alignment
            , isDisabled = viewCustomConfiguration.isDisabled
            , menuWidth = viewCustomConfiguration.menuWidth
            , entries =
                [ Menu.entry "see-more-button" <|
                    \attrs ->
                        ClickableText.button "See more"
                            [ ClickableText.onClick (ConsoleLog "See more")
                            , ClickableText.small
                            , ClickableText.custom attrs
                            , ClickableText.icon UiIcon.seeMore
                            ]
                ]
            }
          <|
            \buttonAttributes ->
                Tooltip.view
                    { trigger =
                        \attrs ->
                            ClickableSvg.button "Menu.viewCustom: Click me!"
                                viewCustomConfiguration.icon
                                [ ClickableSvg.disabled viewCustomConfiguration.isDisabled
                                , ClickableSvg.custom (attrs ++ buttonAttributes)
                                , ClickableSvg.exactWidth 25
                                , ClickableSvg.exactHeight 25
                                , ClickableSvg.css [ Css.marginLeft (Css.px 10) ]
                                ]
                    , id = "viewCustom-example-tooltip"
                    }
                    [ Tooltip.plaintext "Menu.viewCustom: Click me!"
                    , Tooltip.primaryLabel
                    , Tooltip.onHover (ShowTooltip "viewCustom")
                    , Tooltip.open (Set.member "viewCustom" state.openTooltips)
                    , Tooltip.smallPadding
                    , Tooltip.fitToContent
                    ]
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
    , viewCustomConfiguration = initIconButtonWithMenuConfiguration
    }


{-| -}
type alias State =
    { openMenu : Maybe Id
    , checkboxChecked : Bool
    , openTooltips : Set String
    , viewConfiguration : Control ViewConfiguration
    , viewCustomConfiguration : Control IconButtonWithMenuConfiguration
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
    = ShowTooltip String Bool
    | ConsoleLog String
    | SetViewConfiguration (Control ViewConfiguration)
    | SetIconButtonWithMenuConfiguration (Control IconButtonWithMenuConfiguration)
    | FocusAndToggle String { isOpen : Bool, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
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
            ( { state | viewCustomConfiguration = configuration }, Cmd.none )

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

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Id =
    String


type alias Value =
    String
