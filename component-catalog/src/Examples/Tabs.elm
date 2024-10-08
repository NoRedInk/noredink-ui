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
import Debug.Control.Extra exposing (values)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Guidance
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import List.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Message.V4 as Message
import Nri.Ui.Panel.V1 as Panel
import Nri.Ui.Tabs.V9 as Tabs exposing (Alignment(..), Tab)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Task


moduleName : String
moduleName =
    "Tabs"


version : Int
version =
    9


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout, Tabs ]
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
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Tabs component to give Component Catalog users a sense of what the
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
    , about = [ Guidance.communicateState moduleName ]
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings

                tabs =
                    allTabs
                        { openTooltipId = model.openTooltip
                        , withTooltips = settings.withTooltips
                        , labelSource = settings.labelSource
                        , pageBackgroundColor = settings.pageBackgroundColor
                        }
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = SetSettings
                , settings = model.settings
                , mainType = Just "RootHtml.Html { select : Int, focus : Maybe String }"
                , extraCode = [ "import Nri.Ui.Tooltip.V3 as Tooltip" ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        let
                            code =
                                [ moduleName ++ ".view"
                                , "    { focusAndSelect = identity"
                                , "    , selected = " ++ String.fromInt model.selected
                                , "    }"
                                , Code.listMultiline
                                    (List.filterMap identity
                                        [ Just (moduleName ++ ".alignment " ++ moduleName ++ "." ++ Debug.toString settings.alignment)
                                        , Maybe.map (\title -> moduleName ++ ".title " ++ Code.string title) settings.title
                                        , Maybe.map (\spacing -> moduleName ++ ".spacing " ++ String.fromFloat spacing) settings.customSpacing
                                        , Maybe.map (\color -> moduleName ++ ".pageBackgroundColor " ++ colorToCode color) settings.pageBackgroundColor
                                        , Maybe.map
                                            (\sticky ->
                                                case sticky of
                                                    Default ->
                                                        moduleName ++ ".tabsListSticky"

                                                    Custom stickyConfig ->
                                                        moduleName
                                                            ++ ".tabsListStickyCustom "
                                                            ++ Code.recordMultiline
                                                                [ ( "topOffset", String.fromFloat stickyConfig.topOffset )
                                                                , ( "topPadding", String.fromFloat stickyConfig.topPadding )
                                                                , ( "zIndex", String.fromInt stickyConfig.zIndex )
                                                                ]
                                                                2
                                            )
                                            settings.stickiness
                                        ]
                                    )
                                    1
                                , Code.listMultiline (List.map Tuple.first tabs) 1
                                ]
                                    |> String.join "\n"
                        in
                        [ { sectionName = "Example"
                          , code = code
                          }
                        ]
                }
            , Html.div
                [ css
                    [ Css.padding (Css.px 20)
                    , case settings.pageBackgroundColor of
                        Nothing ->
                            Css.batch []

                        Just color ->
                            Css.backgroundColor (colorToCss color)
                    ]
                ]
                [ case settings.pageBackgroundColor of
                    Nothing ->
                        Html.text ""

                    Just _ ->
                        Message.view
                            [ Message.tip
                            , Message.plaintext "Container and tab panel background adjusted to match pageBackgroundColor. Note that the component won't do this for you automatically!"
                            , Message.css [ Css.marginBottom (Css.px 20) ]
                            ]
                , Tabs.view
                    { focusAndSelect = FocusAndSelectTab
                    , selected = model.selected
                    }
                    (List.filterMap identity
                        [ Just (Tabs.alignment settings.alignment)
                        , Maybe.map Tabs.title settings.title
                        , Maybe.map Tabs.spacing settings.customSpacing
                        , Maybe.map (Tabs.pageBackgroundColor << colorToCss) settings.pageBackgroundColor
                        , Maybe.map
                            (\stickiness ->
                                case stickiness of
                                    Default ->
                                        Tabs.tabListSticky

                                    Custom stickyConfig ->
                                        Tabs.tabListStickyCustom stickyConfig
                            )
                            settings.stickiness
                        ]
                    )
                    (List.map Tuple.second tabs)
                ]
            ]
    }


allTabs :
    { openTooltipId : Maybe Int
    , withTooltips : Bool
    , labelSource : LabelSource
    , pageBackgroundColor : Maybe Color
    }
    -> List ( String, Tab Int Msg )
allTabs config =
    List.repeat 4 ()
        |> List.indexedMap (\i _ -> buildTab config i)


buildTab :
    { openTooltipId : Maybe Int
    , withTooltips : Bool
    , labelSource : LabelSource
    , pageBackgroundColor : Maybe Color
    }
    -> Int
    -> ( String, Tab Int Msg )
buildTab config id =
    let
        idString =
            String.fromInt (id + 1)

        tabIdString =
            "tab-" ++ idString

        tabName =
            "Tab " ++ idString

        panelName =
            "Panel " ++ idString

        labelledById =
            idString ++ "-label"

        fixedLabelOverride =
            tabName ++ " custom label"
    in
    ( String.join ""
        [ "Tabs.build { id = " ++ String.fromInt id ++ ", idString = " ++ Code.string tabIdString ++ " }"
        , "\n\t    [ Tabs.tabString " ++ Code.string tabName
        , "\n\t    , Tabs.panelHtml (text " ++ Code.string panelName ++ ")"
        , if config.withTooltips then
            String.join "\n\t    "
                [ "\n\t    , Tabs.withTooltip"
                , "   [ Tooltip.plaintext " ++ Code.string tabName
                , "    -- You will need to have a tooltip handler"
                , "    -- , Tooltip.onToggle ToggleTooltip " ++ ""
                , "   , Tooltip.open " ++ Code.bool (config.openTooltipId == Just id)
                , "   ]"
                ]

          else
            ""
        , case config.labelSource of
            FromInnerText ->
                ""

            LabelledBy ->
                "\n\t    , Tabs.labelledBy " ++ Code.string labelledById

            FixedLabel ->
                "\n\t    , Tabs.label " ++ Code.string fixedLabelOverride
        , "\n\t    ]"
        ]
    , Tabs.build { id = id, idString = tabIdString }
        (List.concat
            [ [ Tabs.tabString tabName
              , Tabs.panelHtml (panelContent config.pageBackgroundColor id panelName)
              ]
            , if config.withTooltips then
                [ Tabs.withTooltip
                    [ Tooltip.plaintext tabName
                    , Tooltip.onToggle (ToggleTooltip id)
                    , Tooltip.open (config.openTooltipId == Just id)
                    ]
                ]

              else
                []
            , case config.labelSource of
                FromInnerText ->
                    []

                LabelledBy ->
                    [ Tabs.labelledBy labelledById ]

                FixedLabel ->
                    [ Tabs.label fixedLabelOverride ]
            ]
        )
    )


panelContent : Maybe Color -> Int -> String -> Html.Html msg
panelContent pageBackgroundColor_ id panelName =
    let
        pangrams =
            -- cycle panels so that panel contents change when changing tabs
            -- without getting too creative :-D
            [ ( "The one about the fox"
              , "The quick brown fox jumps over the lazy dog."
              )
            , ( "The one about the wizards"
              , "The five boxing wizards jump quickly."
              )
            , ( "The one about the zebras"
              , "How quickly daft jumping zebras vex!"
              )
            , ( "The one about the sphinxes"
              , "Sphinx of black quartz, judge my vow."
              )
            ]
                |> List.Extra.splitAt id
                |> (\( beforeSplit, afterSplit ) -> afterSplit ++ beforeSplit)
    in
    Html.div []
        (List.concat
            [ List.map
                (\( title, content ) ->
                    Panel.view
                        [ Panel.header title
                        , Panel.paragraph content
                        , Panel.containerCss [ Css.margin2 (Css.px 10) Css.zero ]
                        ]
                )
                pangrams
            ]
        )


type alias State =
    { selected : Int
    , settings : Control Settings
    , openTooltip : Maybe Int
    }


init : State
init =
    { selected = 0
    , settings = initSettings
    , openTooltip = Nothing
    }


type alias Settings =
    { title : Maybe String
    , alignment : Alignment
    , customSpacing : Maybe Float
    , withTooltips : Bool
    , pageBackgroundColor : Maybe Color
    , stickiness : Maybe Stickiness
    , labelSource : LabelSource
    }


type LabelSource
    = FromInnerText
    | LabelledBy
    | FixedLabel


type Color
    = White
    | Gray


colorToCss : Color -> Css.Color
colorToCss color =
    case color of
        White ->
            Colors.white

        Gray ->
            Colors.gray96


colorToCode : Color -> String
colorToCode color =
    case color of
        White ->
            "Colors.white"

        Gray ->
            "Colors.gray96"


type Stickiness
    = Default
    | Custom Tabs.TabListStickyConfig


initSettings : Control Settings
initSettings =
    let
        colorChoices =
            Control.choice
                [ ( "Gray", Control.value Gray )
                , ( "White", Control.value White )
                ]
    in
    Control.record Settings
        |> Control.field "title" (Control.maybe False (Control.string "Title"))
        |> Control.field "alignment"
            (Control.choice
                [ ( "Left", Control.value Left )
                , ( "Center", Control.value Center )
                , ( "Right", Control.value Right )
                ]
            )
        |> Control.field "customSpacing" (Control.maybe False (values String.fromFloat [ 2, 3, 4, 8, 16 ]))
        |> Control.field "withTooltips" (Control.bool True)
        |> Control.field "pageBackgroundColor" (Control.maybe False colorChoices)
        |> Control.field "tabListSticky"
            (Control.maybe False
                (Control.choice
                    [ ( "Default", Control.value Default )
                    , ( "Custom"
                      , Control.record Tabs.TabListStickyConfig
                            |> Control.field "topOffset" (values String.fromFloat [ 0, 10, 50 ])
                            |> Control.field "topPadding" (values String.fromFloat [ 0, 10, 50 ])
                            |> Control.field "zIndex" (values String.fromInt [ 0, 1, 5, 10 ])
                            |> Control.map Custom
                      )
                    ]
                )
                |> Control.revealed "Tablist Sticky"
            )
        |> Control.field "accessible label"
            (Control.choice
                [ ( "From inner text (default)", Control.value FromInnerText )
                , ( "Labelled by another element", Control.value LabelledBy )
                , ( "Fixed", Control.value FixedLabel )
                ]
            )


type Msg
    = FocusAndSelectTab { select : Int, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)
    | ToggleTooltip Int Bool


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
