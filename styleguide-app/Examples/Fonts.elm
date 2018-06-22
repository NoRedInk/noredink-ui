module Examples.Fonts exposing (example)

{-|

@docs example

-}

import HeadingsStyled as Headings
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Fonts.V1 as Fonts


{-| -}
example : ModuleExample msg
example =
    { filename = "Nri.Ui.Fonts.V1.elm"
    , category = Fonts
    , content =
        [ Headings.h3 [ Html.text "baseFont" ]
        , Html.p [ css [ Fonts.baseFont ] ]
            [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
        , Headings.h3 [ Html.text "quizFont" ]
        , Html.p [ css [ Fonts.quizFont ] ]
            [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
        , Headings.h3 [ Html.text "ugFont" ]
        , Html.p [ css [ Fonts.ugFont ] ]
            [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
        ]
            |> List.map Html.toUnstyled
    }
