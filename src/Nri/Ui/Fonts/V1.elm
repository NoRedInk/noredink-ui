module Nri.Ui.Fonts.V1
    exposing
        ( baseFont
        , quizFont
        )

{-| Fonts for NoRedInk projects

@docs baseFont, quizFont

-}

import Css exposing (..)


{-| Font for instructions, headers, and pretty much everything else
-}
baseFont : Style
baseFont =
    fontFamilies [ qt "Muli", "Helvetica", "Arial", "sans-serif" ]


{-| Font for question sentences, or most interactable or graded fields
-}
quizFont : Style
quizFont =
    fontFamilies [ qt "Georgia", "serif" ]
