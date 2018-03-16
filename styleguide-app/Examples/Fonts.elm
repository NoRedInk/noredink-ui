module Examples.Fonts exposing (example)

{-|

@docs example

-}

import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Fonts.V1 as Fonts


{-| -}
example : ModuleExample msg
example =
    { filename = "Nri.Ui.Fonts.V1.elm"
    , category = Text
    , content =
        [ Html.h3 [ css [ Fonts.baseFont ] ] [ Html.text "baseFont" ]
        , Html.p [ css [ Fonts.baseFont ] ]
            [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
        , Html.h3 [ css [ Fonts.quizFont ] ] [ Html.text "quizFont" ]
        , Html.p [ css [ Fonts.quizFont ] ]
            [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
        ]
            |> List.map Html.toUnstyled
    }
