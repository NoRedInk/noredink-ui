module UsageExamples.MediaQuery exposing (example)

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
    { name = "MediaQuery.V2"
    , categories = [ Category.Layout ]
    , init = ()
    , update = \_ _ -> ( (), Cmd.none )
    , view = always view
    , about = []
    , subscriptions = \_ -> Sub.none
    }


view : List (Html msg)
view =
    viewCascade :: viewIndividually


viewCascade : Html msg
viewCascade =
    let
        content str =
            Css.property "content" ("'" ++ str ++ "'")

        before str =
            Css.before [ content str ]

        after str =
            Css.after [ content str ]
    in
    Container.view
        [ Container.buttony
        , Container.css
            [ Css.displayFlex
            , Css.property "justify-content" "space-evenly"
            , before "Default"
            , after "Default"
            , MediaQuery.fromList
                [ MediaQuery.narrowMobile [ before "Narrow Mobile" ]
                , MediaQuery.quizEngineMobile [ before "Quiz Engine Mobile" ]
                , MediaQuery.mobile [ before "Mobile" ]
                , MediaQuery.not MediaQuery.narrowMobile [ after "Not Narrow Mobile" ]
                , MediaQuery.not MediaQuery.quizEngineMobile [ after "Not Quiz Engine Mobile" ]
                , MediaQuery.not MediaQuery.mobile [ after "Not Mobile" ]
                ]
            ]
        , Container.plaintext " | "
        ]


viewIndividually : List (Html msg)
viewIndividually =
    let
        hidden =
            Css.display Css.none

        visible =
            Css.display Css.block

        viewBreakpoint name breakpoint =
            Container.view
                [ Container.buttony
                , Container.css [ hidden, MediaQuery.fromList [ breakpoint [ visible ] ] ]
                , Container.plaintext name
                ]
    in
    [ viewBreakpoint "Narrow Mobile" MediaQuery.narrowMobile
    , viewBreakpoint "Quiz Engine Mobile" MediaQuery.quizEngineMobile
    , viewBreakpoint "Mobile" MediaQuery.mobile
    , viewBreakpoint "Not Narrow Mobile" (MediaQuery.not MediaQuery.narrowMobile)
    , viewBreakpoint "Not Quiz Engine Mobile" (MediaQuery.not MediaQuery.quizEngineMobile)
    , viewBreakpoint "Not Mobile" (MediaQuery.not MediaQuery.mobile)
    ]
