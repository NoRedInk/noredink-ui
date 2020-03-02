module Nri.Ui.Fonts.V1 exposing (baseFont, quizFont, ugFont)

{-| Fonts for NoRedInk projects

@docs baseFont, quizFont, ugFont

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


{-| Font for displaying user-generated content.
-}
ugFont : Style
ugFont =
    fontFamilies [ qt "Georgia", "serif" ]
