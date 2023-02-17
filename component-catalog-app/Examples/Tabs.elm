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
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Tabs.V7 as Tabs exposing (Alignment(..), Tab)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
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
    , view =
        \ellieLinkConfig model ->
            let
                settings =
                    Control.currentValue model.settings

                tabs =
                    allTabs model.openTooltip settings.withTooltips
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
                                , "    { title = " ++ Code.maybeString settings.title
                                , "    , alignment = " ++ moduleName ++ "." ++ Debug.toString settings.alignment
                                , "    , customSpacing = " ++ Code.maybeFloat settings.customSpacing
                                , "    , focusAndSelect = identity"
                                , "    , selected = " ++ String.fromInt model.selected
                                , "    , tabs = " ++ Code.listMultiline (List.map Tuple.first tabs) 2
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
                , tabs = List.map Tuple.second tabs
                }
            ]
    }


allTabs : Maybe Int -> Bool -> List ( String, Tab Int Msg )
allTabs openTooltipId withTooltips =
    List.repeat 4 ()
        |> List.indexedMap (\i _ -> buildTooltip openTooltipId withTooltips i)


buildTooltip : Maybe Int -> Bool -> Int -> ( String, Tab Int Msg )
buildTooltip openTooltipId withTooltips id =
    let
        idString =
            String.fromInt (id + 1)

        tabIdString =
            "tab-" ++ idString

        tabName =
            "Tab " ++ idString

        panelName =
            "Panel " ++ idString
    in
    ( String.join ""
        [ "Tabs.build { id = " ++ String.fromInt id ++ ", idString = " ++ Code.string tabIdString ++ " }"
        , "\n\t    [ Tabs.tabString " ++ Code.string tabName
        , "\n\t    , Tabs.panelHtml (text " ++ Code.string panelName ++ ")"
        , if withTooltips then
            String.join "\n\t    "
                [ "\n\t    , Tabs.withTooltip"
                , "   [ Tooltip.plaintext " ++ Code.string tabName
                , "    -- You will need to have a tooltip handler"
                , "    -- , Tooltip.onToggle ToggleTooltip " ++ ""
                , "   , Tooltip.open " ++ Code.bool (openTooltipId == Just id)
                , "   ]"
                ]

          else
            ""
        , "\n\t    ]"
        ]
    , Tabs.build { id = id, idString = tabIdString }
        ([ Tabs.tabString tabName
         , Tabs.panelHtml (Html.text panelName)
         ]
            ++ (if withTooltips then
                    [ Tabs.withTooltip
                        [ Tooltip.plaintext tabName
                        , Tooltip.onToggle (ToggleTooltip id)
                        , Tooltip.open (openTooltipId == Just id)
                        ]
                    ]

                else
                    []
               )
        )
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
        |> Control.field "withTooltips" (Control.bool True)


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