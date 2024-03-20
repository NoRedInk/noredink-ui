module UsageExamples.MediaQueryBuilder exposing (example)

{-|

    @docs example

-}

import Category
import Css
import Html.Styled exposing (..)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.MediaQuery.V2 as MediaQuery
import UsageExample exposing (UsageExample)


example : UsageExample () ()
example =
    { name = "MediaQueryBuilder"
    , categories = [ Category.Layout ]
    , init = ()
    , update = \_ _ -> ( (), Cmd.none )
    , view = always view
    , about = []
    , subscriptions = \_ -> Sub.none
    }


view : List (Html msg)
view =
    let
        content str =
            Css.property "content" ("'" ++ str ++ "'")

        before str =
            Css.before [ content str ]

        after str =
            Css.after [ content str ]
    in
    [ h2 [] [ text "MediaQueryBuilder" ]
    , Container.view
        [ Container.buttony
        , Container.css
            (Css.displayFlex
                :: Css.property "justify-content" "space-evenly"
                :: before "Default"
                :: after "Default"
                :: MediaQuery.styles
                    [ MediaQuery.narrowMobile [ before "Narrow Mobile" ]
                    , MediaQuery.quizEngineMobile [ before "Quiz Engine Mobile" ]
                    , MediaQuery.mobile [ before "Mobile" ]
                    , MediaQuery.notNarrowMobile [ after "Not Narrow Mobile" ]
                    , MediaQuery.notQuizEngineMobile [ after "Not Quiz Engine Mobile" ]
                    , MediaQuery.notMobile [ after "Not Mobile" ]
                    ]
            )
        , Container.plaintext " | "
        ]
    ]
