module Examples.HighlightIcon exposing (example)

{-|

@docs example

-}

import Examples.IconExamples as IconExamples
import ModuleExample exposing (Category(..), ModuleExample)
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
        ]
    }
