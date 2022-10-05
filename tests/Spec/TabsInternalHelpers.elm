module Spec.TabsInternalHelpers exposing (..)

import Accessibility.Key as Key
import Accessibility.Role as Role
import Browser.Dom as Dom
import Expect
import Html.Styled as Html exposing (..)
import ProgramTest exposing (..)
import Spec.KeyboardHelpers as KeyboardHelpers
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


type alias State =
    { selected : Int
    }


init : State
init =
    { selected = 0
    }


type Msg
    = FocusAndSelectTab { select : Int, focus : Maybe String }
    | Focused (Result Dom.Error ())


type alias TestContext =
    ProgramTest State Msg ()


ensureTabbable : String -> TestContext -> TestContext
ensureTabbable word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
                >> Query.has [ Selector.text word ]
            )


ensurePanelsFocusable : List String -> TestContext -> TestContext
ensurePanelsFocusable words testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.attribute (Key.tabbable True) ]
                >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) words)
            )


ensurePanelDisplayed : String -> TestContext -> TestContext
ensurePanelDisplayed word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tabPanel, Selector.style "display" "block" ]
                >> Query.has [ Selector.text word ]
            )


ensureOnlyOnePanelDisplayed : List String -> TestContext -> TestContext
ensureOnlyOnePanelDisplayed panels testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.style "display" "block" ]
                >> Query.count (Expect.equal 1)
            )
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.style "display" "none" ]
                >> Query.count (Expect.equal (List.length panels - 1))
            )


ensureOnlyOneTabInSequence : List String -> TestContext -> TestContext
ensureOnlyOneTabInSequence tabs testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
                >> Query.count (Expect.equal 1)
            )
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable False) ]
                >> Query.count (Expect.equal (List.length tabs - 1))
            )


releaseRightArrow : TestContext -> TestContext
releaseRightArrow =
    KeyboardHelpers.releaseRightArrow { targetDetails = [] }
        [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]


releaseLeftArrow : TestContext -> TestContext
releaseLeftArrow =
    KeyboardHelpers.releaseLeftArrow { targetDetails = [] }
        [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
