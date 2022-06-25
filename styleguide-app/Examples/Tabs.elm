module Examples.Tabs exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled as Html exposing (fromUnstyled)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Tabs.V7 as Tabs exposing (Alignment(..), Tab)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


moduleName : String
moduleName =
    "Tabs"


version : Int
version =
    7


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout ]
    , keyboardSupport =
        [ { keys = [ KeyboardSupport.Tab ]
          , result = "Move focus to the currently-selected Tab's tab panel"
          }
        , { keys = [ Arrow KeyboardSupport.Left ]
          , result = "Select the tab to the left of the currently-selected Tab"
          }
        , { keys = [ Arrow KeyboardSupport.Right ]
          , result = "Select the tab to the right of the currently-selected Tab"
          }
        ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Tabs component to give styleguide users a sense of what the
          -- component might look like
          Html.div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
            [ Html.div
                [ css
                    [ Css.backgroundColor Colors.white
                    , Css.padding (Css.px 4)
                    , Css.borderRadius4 (Css.px 4) (Css.px 4) Css.zero Css.zero
                    , Css.border3 (Css.px 1) Css.solid Colors.navy
                    , Css.borderBottomWidth Css.zero
                    ]
                ]
                [ Text.smallBody [ Text.plaintext "Tab 1" ] ]
            , Html.div
                [ css [ Css.width (Css.px 4), Css.borderBottom3 (Css.px 1) Css.solid Colors.navy ]
                ]
                []
            , Html.div
                [ css
                    [ Css.backgroundColor Colors.frost
                    , Css.padding (Css.px 4)
                    , Css.borderRadius4 (Css.px 4) (Css.px 4) Css.zero Css.zero
                    , Css.border3 (Css.px 1) Css.solid Colors.navy
                    ]
                ]
                [ Text.smallBody [ Text.plaintext "Tab 1" ] ]
            , Html.div
                [ css
                    [ Css.width (Css.px 30)
                    , Css.borderBottom3 (Css.px 1) Css.solid Colors.navy
                    ]
                ]
                []
            , Html.div
                [ css
                    [ Css.paddingTop (Css.px 4)
                    , Css.minWidth (Css.px 100)
                    ]
                ]
                [ Text.caption [ Text.plaintext "Tab 1 content" ] ]
            ]
        ]
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = SetSettings
                , settings = model.settings
                , mainType = "RootHtml.Html { select : Int, focus : Maybe String }"
                , extraImports = []
                , toExampleCode =
                    \_ ->
                        let
                            code =
                                [ moduleName ++ ".view"
                                , "    { title = " ++ Code.maybeString settings.title
                                , "    , alignment = " ++ moduleName ++ "." ++ Debug.toString settings.alignment
                                , "    , customSpacing = " ++ " TODO"
                                , "    , focusAndSelect = " ++ " TODO"
                                , "    , selected = " ++ " TODO"
                                , "    , tabs = " ++ " TODO"
                                , "    }"
                                ]
                                    |> String.join "\n"
                        in
                        [ { sectionName = "Example"
                          , code = code
                          }
                        ]
                }
            , Tabs.view
                { title = settings.title
                , alignment = settings.alignment
                , customSpacing = settings.customSpacing
                , focusAndSelect = FocusAndSelectTab
                , selected = model.selected
                , tabs = allTabs model.openTooltip settings.labelledBy
                }
            ]
    }


allTabs : Maybe Id -> Maybe String -> List (Tab Id Msg)
allTabs openTooltipId labelledBy =
    let
        bulbIcon =
            UiIcon.bulb
                |> Svg.withWidth (Css.px 40)
                |> Svg.withHeight (Css.px 45)
                |> Svg.withLabel "Bulb"
                |> Svg.withCss [ Css.padding2 Css.zero (Css.px 6) ]
                |> Svg.toHtml
    in
    [ Tabs.build { id = First, idString = "tab-0" }
        ([ Tabs.tabString "1"
         , Tabs.withTooltip
            [ Tooltip.plaintext "Link Example"
            , Tooltip.onToggle (ToggleTooltip First)
            , Tooltip.alignStart (Css.px 75)
            , Tooltip.primaryLabel
            , Tooltip.open (openTooltipId == Just First)
            ]
         , Tabs.panelHtml (Html.text "First Panel")
         ]
            ++ (case labelledBy of
                    Nothing ->
                        []

                    Just labelledById ->
                        [ Tabs.labelledBy labelledById ]
               )
        )
    , Tabs.build { id = Second, idString = "tab-1" }
        [ Tabs.tabString "Second Tab (disabled)"
        , Tabs.disabled True
        , Tabs.panelHtml (Html.text "Second Panel")
        ]
    , Tabs.build { id = Third, idString = "tab-2" }
        [ Tabs.tabHtml bulbIcon
        , Tabs.withTooltip
            [ Tooltip.plaintext "The Electrifying Third Tab"
            , Tooltip.onToggle (ToggleTooltip Third)
            , Tooltip.primaryLabel
            , Tooltip.open (openTooltipId == Just Third)
            ]
        , Tabs.panelHtml (Html.text "Third Panel")
        ]
    , Tabs.build { id = Fourth, idString = "tab-3" }
        [ Tabs.tabString "Fourth Tab"
        , Tabs.panelHtml (Html.text "Fourth Panel")
        ]
    ]


type alias State =
    { selected : Id
    , settings : Control Settings
    , openTooltip : Maybe Id
    }


init : State
init =
    { selected = First
    , settings = initSettings
    , openTooltip = Nothing
    }


type alias Settings =
    { title : Maybe String
    , alignment : Alignment
    , customSpacing : Maybe Float
    , labelledBy : Maybe String
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "title" (Control.maybe False (Control.string "Title"))
        |> Control.field "alignment"
            (Control.choice
                [ ( "Left", Control.value Left )
                , ( "Center", Control.value Center )
                , ( "Right", Control.value Right )
                ]
            )
        |> Control.field "customSpacing"
            (Control.maybe False
                (Control.choice
                    [ ( "2", Control.value 2 )
                    , ( "3", Control.value 3 )
                    , ( "4", Control.value 4 )
                    , ( "8", Control.value 8 )
                    , ( "16", Control.value 16 )
                    ]
                )
            )
        |> Control.field "labelledBy" (Control.maybe False (Control.string "someId"))


type Id
    = First
    | Second
    | Third
    | Fourth


type Msg
    = FocusAndSelectTab { select : Id, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)
    | ToggleTooltip Id Bool


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        FocusAndSelectTab { select, focus } ->
            ( { model | selected = select }
            , focus
                |> Maybe.map (Dom.focus >> Task.attempt Focused)
                |> Maybe.withDefault Cmd.none
            )

        Focused error ->
            ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Cmd.none )

        ToggleTooltip id openTooltip ->
            ( { model
                | openTooltip =
                    if openTooltip then
                        Just id

                    else
                        Nothing
              }
            , Cmd.none
            )
