module Nri.Ui.Text.Quiz.V1
    exposing
        ( footnote
        )

{-| Text types for quizzes:

@docs footnote

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Html exposing (..)
import Nri.Colors exposing (..)
import Nri.Fonts exposing (quizFont)
import Nri.Ui.Styles.V1


{-| This is a little note or footnote.
-}
footnote : List (Html msg) -> Html msg
footnote content =
    p [ class footnoteClasses ] content


footnoteClasses : List CssClasses
footnoteClasses =
    [ Footnote ]


namespace : String
namespace =
    "Nri-Ui-Text-Quiz-"


type CssClasses
    = Footnote


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles namespace
        [ Css.Foreign.class Footnote
            [ makeQuizFont (px 13) gray45
            , lineHeight (px 18)
            , fontWeight (int 400)
            , margin4 (px 5) (px 0) (px 0) (px 0)
            ]
        ]


makeQuizFont : Css.FontSize a -> Css.ColorValue b -> Style
makeQuizFont size fontColor =
    Css.batch
        [ quizFont
        , fontSize size
        , color fontColor
        ]


{ class } =
    styles


{-| Eliminate widows (single words on their own line caused by
wrapping) by inserting a non-breaking space if there are at least two
words.
-}
noWidow : String -> String
noWidow inputs =
    let
        -- this value is a unicode non-breaking space since Elm
        -- doesn't support named character entities
        nbsp =
            " "

        words =
            String.split " " inputs

        insertPoint =
            List.length words - 1
    in
    words
        |> List.indexedMap
            (\i word ->
                if i == 0 then
                    word
                else if i == insertPoint && insertPoint > 0 then
                    nbsp ++ word
                else
                    " " ++ word
            )
        |> String.join ""
