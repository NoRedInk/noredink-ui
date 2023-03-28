module Spec.Nri.Ui.HighlighterToolbar exposing (..)

import Css
import Expect
import Html.Attributes as Attributes
import Html.Styled as Html exposing (..)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlighterToolbar.V3 as HighlighterToolbar
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


spec : Test
spec =
    describe "Nri.Ui.HighlighterToolbar.V3"
        [ test "sets selection status on the active tool" <|
            \() ->
                program
                    |> clickTool "Claim"
                    |> ensureActiveToolIs "Claim"
                    |> clickTool "Evidence"
                    |> ensureActiveToolIs "Evidence"
                    |> done
        ]


byLabel : String -> List Selector
byLabel label =
    [ Selector.tag "label"
    , Selector.containing [ Selector.text label ]
    ]


activeToolVisualIndicator : List Selector
activeToolVisualIndicator =
    [ Selector.attribute (Attributes.attribute "data-nri-description" "active-tool") ]


clickTool : String -> ProgramTest model msg effect -> ProgramTest model msg effect
clickTool label =
    ProgramTest.within (Query.find (byLabel label))
        (ProgramTest.simulateDomEvent
            (Query.find [ Selector.tag "input" ])
            Event.click
        )


ensureActiveToolIs : String -> ProgramTest model msg effect -> ProgramTest model msg effect
ensureActiveToolIs label testContext =
    testContext
        |> ProgramTest.within (Query.find (byLabel label))
            -- has the attribute showing the radio as checked
            (ProgramTest.ensureViewHas [ Selector.attribute (Attributes.checked True) ]
                -- has a visual indicator of selection
                >> ProgramTest.ensureViewHas activeToolVisualIndicator
            )
        |> ProgramTest.ensureView
            -- has only 1 checked radio
            (Query.findAll [ Selector.attribute (Attributes.checked True) ]
                >> Query.count (Expect.equal 1)
            )
        |> ProgramTest.ensureView
            -- has only 1 visual indicator of selection
            (Query.findAll activeToolVisualIndicator
                >> Query.count (Expect.equal 1)
            )


type alias Tag =
    { name : String
    , colorSolid : Css.Color
    , colorLight : Css.Color
    }


tags : List Tag
tags =
    [ { name = "Claim"
      , colorSolid = Colors.mustard
      , colorLight = Colors.highlightYellow
      }
    , { name = "Evidence"
      , colorSolid = Colors.magenta
      , colorLight = Colors.highlightMagenta
      }
    , { name = "Reasoning"
      , colorSolid = Colors.cyan
      , colorLight = Colors.highlightCyan
      }
    ]


view : Maybe Tag -> Html (Maybe Tag)
view model =
    HighlighterToolbar.view
        { onSelect = identity
        , getNameAndColor = identity
        , highlighterId = "highlighter"
        }
        { currentTool = model
        , tags = tags
        }


program : ProgramTest (Maybe Tag) (Maybe Tag) ()
program =
    ProgramTest.createSandbox
        { init = Nothing
        , update = \new _ -> new
        , view = view >> Html.toUnstyled
        }
        |> ProgramTest.start ()
