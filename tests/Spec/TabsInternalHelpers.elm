module Spec.TabsInternalHelpers exposing (..)

import Accessibility.Key as Key
import Accessibility.Role as Role
import Browser.Dom as Dom
import Expect
import Html.Styled exposing (..)
import Nri.Test.KeyboardHelpers.V1 as KeyboardHelpers
import ProgramTest exposing (..)
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


ensureTabbable : String -> ProgramTest model msg effect -> ProgramTest model msg effect
ensureTabbable word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
                >> Query.has [ Selector.text word ]
            )


ensurePanelsFocusable : List String -> ProgramTest model msg effect -> ProgramTest model msg effect
ensurePanelsFocusable words testContext =
    testContext
        |> ensureView
            (Query.findAll [ Selector.attribute Role.tabPanel, Selector.attribute (Key.tabbable True) ]
                >> Expect.all (List.indexedMap (\i w -> Query.index i >> Query.has [ Selector.text w ]) words)
            )


ensurePanelDisplayed : String -> ProgramTest model msg effect -> ProgramTest model msg effect
ensurePanelDisplayed word testContext =
    testContext
        |> ensureView
            (Query.find [ Selector.attribute Role.tabPanel, Selector.style "display" "block" ]
                >> Query.has [ Selector.text word ]
            )


ensureOnlyOnePanelDisplayed : List String -> ProgramTest model msg effect -> ProgramTest model msg effect
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


ensureOnlyOneTabInSequence : List String -> ProgramTest model msg effect -> ProgramTest model msg effect
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


releaseRightArrow : ProgramTest model msg effect -> ProgramTest model msg effect
releaseRightArrow =
    KeyboardHelpers.releaseRightArrow
        { targetDetails = [] }
        [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]


releaseLeftArrow : ProgramTest model msg effect -> ProgramTest model msg effect
releaseLeftArrow =
    KeyboardHelpers.releaseLeftArrow
        { targetDetails = [] }
        [ Selector.attribute Role.tab, Selector.attribute (Key.tabbable True) ]
