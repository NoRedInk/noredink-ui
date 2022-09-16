module Nri.Ui.Text.Writing.V1 exposing (footnote)

{-| DEPRECATED: use Nri.Ui.Text.V\*.footnote instead.

@docs footnote

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 exposing (quizFont)


{-| This is a little note or footnote.
-}
footnote : List (Html msg) -> Html msg
footnote =
    styled p
        [ makeWritingFont (px 13) gray45
        , lineHeight (px 18)
        , fontWeight (int 400)
        , margin4 (px 5) (px 0) (px 0) (px 0)
        ]
        []


makeWritingFont : Css.FontSize a -> Css.ColorValue b -> Style
makeWritingFont size fontColor =
    Css.batch
        [ quizFont
        , fontSize size
        , color fontColor
        ]
