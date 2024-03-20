module MediaQuerySpec exposing (suite)

import Css exposing (borderWidth, fontSize, int, order, px)
import Fuzz exposing (Fuzzer)
import Html.Styled exposing (div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.MediaQuery.V2 as MediaQuery
    exposing
        ( MediaQuery
        , highContrastMode
        , mobile
        , narrowMobile
        , prefersReducedMotion
        , quizEngineMobile
        )
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


allQueriesRandomOrderFuzzer : Fuzzer (List MediaQuery)
allQueriesRandomOrderFuzzer =
    Fuzz.shuffledList
        [ MediaQuery.not narrowMobile [ order (int 1) ]
        , MediaQuery.not quizEngineMobile [ order (int 2) ]
        , MediaQuery.not mobile [ order (int 3) ]
        , mobile [ order (int 4) ]
        , quizEngineMobile [ order (int 5) ]
        , narrowMobile [ order (int 6) ]
        , highContrastMode [ order (int -1) ]
        , MediaQuery.not highContrastMode [ order (int -11) ]
        , prefersReducedMotion [ order (int -2) ]
        , MediaQuery.not prefersReducedMotion [ order (int -22) ]
        ]


suite : Test
suite =
    describe "MediaQuery.builder"
        [ fuzz allQueriesRandomOrderFuzzer "it puts queries in the correct order" <|
            \queries ->
                div [ css <| MediaQuery.toStyles queries ] []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text (String.trim """
@media only screen and (min-width: 501px){._28601c77{order:1;}}
@media only screen and (min-width: 751px){._28601c77{order:2;}}
@media only screen and (min-width: 1001px){._28601c77{order:3;}}
@media (forced-colors: none){._28601c77{order:-11;}}
@media (forced-colors: active){._28601c77{order:-1;}}
@media (prefers-reduced-motion: no-preference){._28601c77{order:-22;}}
@media (prefers-reduced-motion){._28601c77{order:-2;}}
@media only screen and (max-width: 1000px){._28601c77{order:4;}}
@media only screen and (max-width: 750px){._28601c77{order:5;}}
@media only screen and (max-width: 500px){._28601c77{order:6;}}
                    """)
                        ]
        , test "it works with duplicated queries" <|
            \() ->
                div
                    [ css <|
                        MediaQuery.toStyles
                            [ mobile [ borderWidth (px 1) ]
                            , mobile [ fontSize (px 1) ]
                            ]
                    ]
                    []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text "border-width:1px;"
                        , Selector.text "font-size:1px;"
                        ]
        ]
