module Spec.Nri.Ui.Tabs.V5 exposing (all)

import Accessibility.Styled as Html
import Expect
import List.Zipper.Extra
import Nri.Ui.Tabs.V5 as Tabs
import ProgramTest
import Test exposing (..)


all : Test
all =
    describe "Nri.Ui.Tabs.V5"
        [ test "works with ProgramTest.clickButton" <|
            \() ->
                ProgramTest.createSandbox
                    { init = Err "No msg"
                    , update = \newResult _ -> newResult
                    , view =
                        \_ ->
                            Tabs.view
                                { title = Nothing
                                , onSelect = Ok
                                , tabs =
                                    List.Zipper.Extra.from []
                                        (Tabs.Tab "First tab" "ID_FIRST")
                                        [ Tabs.Tab "Second tab" "ID_SECOND" ]
                                , content = \_ -> Html.text ""
                                , alignment = Tabs.Center
                                }
                                |> Html.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ProgramTest.clickButton "Second tab"
                    |> ProgramTest.expectModel (Expect.equal (Ok "ID_SECOND"))
        ]
