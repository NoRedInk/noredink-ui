module Examples.HighlightIcon exposing (example)

{-|

@docs example

-}

import Css exposing (..)
import Examples.IconExamples as IconExamples
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.HighlightIcon.V1 as HighlightIcon


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.HighlightIcon.V1"
    , category = Icons
    , content =
        [ IconExamples.view "Highlight Icons"
            [ ( "highlighter", HighlightIcon.highlighter )
            , ( "eraser", HighlightIcon.eraser )
            ]
        , div [ css [ displayFlex, alignItems center ] ]
            [ text "Hover:"
            , div
                [ css
                    [ border3 (px 1) dashed Colors.gray92
                    , padding (px 20)
                    , margin (px 8)
                    , HighlightIcon.highlighterCursor Colors.highlightPurpleDark
                    ]
                ]
                [ text "highlightCursor"
                ]
            , div
                [ css
                    [ border3 (px 1) dashed Colors.gray92
                    , padding (px 20)
                    , margin (px 8)
                    , HighlightIcon.eraserCursor Colors.highlightPurpleDark
                    ]
                ]
                [ text "eraserCursor"
                ]
            ]
        ]
    }
