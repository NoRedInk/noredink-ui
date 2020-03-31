module Examples.Fonts exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.Fonts.V1"
    , categories = List.singleton Text
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ Heading.h3 [] [ Html.text "baseFont" ]
            , Html.p [ css [ Fonts.baseFont ] ]
                [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
            , Heading.h3 [] [ Html.text "quizFont" ]
            , Html.p [ css [ Fonts.quizFont ] ]
                [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
            , Heading.h3 [] [ Html.text "ugFont" ]
            , Html.p [ css [ Fonts.ugFont ] ]
                [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
            ]
    }
