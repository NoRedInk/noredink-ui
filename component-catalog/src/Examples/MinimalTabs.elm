module Examples.MinimalTabs exposing
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
import List.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MinimalTabs.V1 as MinimalTabs exposing (Tab)
import Nri.Ui.Panel.V1 as Panel
import Nri.Ui.Text.V6 as Text
import Task


moduleName : String
moduleName =
    "MinimalTabs"


version : Int
version =
    1


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
        [ -- faking a mini version of the MinimalTabs component to give Component Catalog users a sense of what the
          -- component might look like
          Html.div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
            [ Html.div
                [ css
                    [ Css.padding (Css.px 4)
                    , Css.borderBottom3 (Css.px 3) Css.solid Colors.gray45
                    ]
                ]
                [ Text.smallBody [ Text.plaintext "Tab 1" ] ]
            , Html.div
                [ css
                    [ Css.padding (Css.px 4)
                    , Css.borderBottom3 (Css.px 2) Css.solid Colors.gray85
                    ]
                ]
                [ Text.smallBody [ Text.plaintext "Tab 2" ] ]
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
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = SetSettings
                , settings = model.settings
                , mainType = Just "RootHtml.Html { select : Int, focus : Maybe String }"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        let
                            code =
                                Code.fromModule moduleName "view"
                                    ++ Code.recordMultiline
                                        [ ( "focusAndSelect", "identity" )
                                        , ( "selected", String.fromInt model.selected )
                                        ]
                                        1
                                    ++ Code.listMultiline (List.map Tuple.first allTabs) 1
                        in
                        [ { sectionName = "Example"
                          , code = code
                          }
                        ]
                }
            , MinimalTabs.view
                { focusAndSelect = FocusAndSelectTab
                , selected = model.selected
                }
                (List.map Tuple.second allTabs)
            ]
    }


allTabs : List ( String, Tab Int Msg )
allTabs =
    List.map buildTab (List.range 0 3)


buildTab :
    Int
    -> ( String, Tab Int Msg )
buildTab id =
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
    ( Code.fromModule moduleName "build "
        ++ Code.record [ ( "id", String.fromInt id ), ( "idString", Code.string tabIdString ) ]
        ++ Code.listMultiline
            [ Code.fromModule moduleName "tabString " ++ Code.string tabName
            , Code.fromModule moduleName "panelHtml "
                ++ Code.withParens ("text " ++ Code.string "Panel Two")
            ]
            2
    , MinimalTabs.build { id = id, idString = tabIdString }
        [ MinimalTabs.tabString tabName
        , MinimalTabs.panelHtml (panelContent id panelName)
        ]
    )


panelContent : Int -> String -> Html.Html msg
panelContent id panelName =
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
    }


init : State
init =
    { selected = 0
    , settings = Control.record ()
    }


type alias Settings =
    ()


type Msg
    = FocusAndSelectTab { select : Int, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | SetSettings (Control Settings)


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
