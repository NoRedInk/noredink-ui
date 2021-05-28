module Examples.Text exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V5 as Text


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Text"
    , version = 5
    , categories = [ Text ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            let
                exampleHtml kind =
                    [ Html.text "This is a "
                    , Html.strong [] [ Html.text kind ]
                    , Html.text ". "
                    , Html.a
                        [ Attributes.href "http://www.noredink.com"
                        , Attributes.target "_blank"
                        ]
                        [ Html.text "The quick brown fox jumps over the lazy dog." ]
                    , Html.text " Be on the lookout for a new and improved assignment creation form! Soon, you'll be able to easily see a summary of the content you're assigning, as well as an estimate for how long the assignment will take."
                    ]

                exampleUGHtml kind =
                    [ Html.text "This is a "
                    , Html.strong [] [ Html.text kind ]
                    , Html.text ". The quick brown fox jumps over the lazy dog."
                    , Html.text " When I stepped out, into the bright sunlight from the darkness of the movie house, I had only two things on my mind: Paul Newman, and a ride home."
                    ]
            in
            [ Text.caption [] [ Html.text "NOTE: When using these styles, please read the documentation in the Elm module about \"Understanding spacing\"" ]
            , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "Paragraph styles" ]
            , Text.mediumBody [] (exampleHtml "mediumBody")
            , Text.smallBody [] (exampleHtml "smallBody")
            , Text.smallBodyGray [] (exampleHtml "smallBodyGray")
            , Text.caption [] (exampleHtml "caption")
            , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "Paragraph styles for user-authored content" ]
            , Text.ugMediumBody [] (exampleUGHtml "ugMediumBody")
            , Text.ugSmallBody [] (exampleUGHtml "ugSmallBody")
            , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "One-Off Styles" ]
            , Text.mediumBody
                [ Text.css [ Css.padding (Css.px 20) ] ]
                [ Html.text "I've got more padding than my siblings!" ]
            , Html.div
                [ Attributes.css
                    [ Css.width (Css.px 80)
                    , Css.border3 (Css.px 1) Css.solid (Css.hex "000")
                    ]
                ]
                [ Text.mediumBody
                    [ Text.noBreak ]
                    [ Html.text "I won't ever break, no matter how narrow my container is." ]
                ]
            ]
    }
