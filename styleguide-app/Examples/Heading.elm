module Examples.Heading exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.Heading.V2"
    , categories = List.singleton Text
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ Heading.h1 [] [ Html.text "This is the main page heading." ]
            , Heading.h2 [] [ Html.text "This is a tagline" ]
            , Heading.h3 [] [ Html.text "This is a subHeading" ]
            , Heading.h4 [] [ Html.text "This is a smallHeading" ]
            , Heading.h2 [ Heading.style Heading.Top ]
                [ Html.text "Heading.h2 [ Heading.style Heading.Top ]" ]
            , Heading.h2 [ Heading.css [ Css.color Colors.highlightPurpleDark ] ]
                [ Html.text "Heading.h2 [ Heading.css [ Css.color Colors.highlightPurpleDark ] ]" ]
            ]
    }
