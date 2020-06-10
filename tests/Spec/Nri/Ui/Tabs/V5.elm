module Spec.Nri.Ui.Tabs.V5 exposing (all)

import Accessibility.Styled as Html
import Expect
import Nri.Ui.Tabs.V5 as Tabs
import ProgramTest
import Test exposing (..)


all : Test
all =
    describe "Nri.Ui.Tabs.V5"
        [ test "works with ProgramTest.clickButton" <|
            \() ->
                ProgramTest.createSandbox
                    { init = "ID_FIRST"
                    , update =
                        \msg old ->
                            case msg of
                                Select id ->
                                    id

                                Focus idString ->
                                    old
                    , view =
                        \selected ->
                            Tabs.view
                                { title = Nothing
                                , alignment = Tabs.Center
                                , customSpacing = Nothing
                                , onSelect = Select
                                , onFocus = Focus
                                , selected = selected
                                , tabs =
                                    [ { id = "ID_FIRST"
                                      , idString = "first"
                                      , spaHref = Nothing
                                      , tabView = Tabs.viewTabDefault "Link example"
                                      , panelView = Html.text "First Panel"
                                      }
                                    , { id = "ID_SECOND"
                                      , idString = "second"
                                      , spaHref = Nothing
                                      , tabView = Tabs.viewTabDefault "Second Tab"
                                      , panelView = Html.text "Second Panel"
                                      }
                                    ]
                                }
                                |> Html.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ProgramTest.clickButton "Second tab"
                    |> ProgramTest.expectModel (Expect.equal "ID_SECOND")
        ]


type Msg
    = Select String
    | Focus String
