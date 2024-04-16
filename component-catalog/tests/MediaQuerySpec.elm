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
        , offset
        , prefersReducedMotion
        , quizEngineMobile
        )
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


allQueriesRandomOrderFuzzer : Fuzzer (List (MediaQuery { offsettable : () }))
allQueriesRandomOrderFuzzer =
    Fuzz.shuffledList
        [ MediaQuery.not narrowMobile [ order (int 1) ]
        , MediaQuery.not quizEngineMobile [ order (int 2) ]
        , MediaQuery.not mobile [ order (int 3) ]
        , MediaQuery.not (offset 100 mobile) [ order (int 4) ]
        , offset -100 mobile [ order (int 6) ]
        , mobile [ order (int 5) ]
        , quizEngineMobile [ order (int 7) ]
        , narrowMobile [ order (int 8) ]
        ]


suite : Test
suite =
    describe "MediaQuery.V2"
        [ fuzz allQueriesRandomOrderFuzzer "it puts breakpoint queries in the correct order" <|
            \queries ->
                div [ css [ MediaQuery.fromList queries ] ] []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text (String.trim """
@media only screen and (min-width: 501px){._a039f42a{order:1;}}
@media only screen and (min-width: 751px){._a039f42a{order:2;}}
@media only screen and (min-width: 1001px){._a039f42a{order:3;}}
@media only screen and (min-width: 1101px){._a039f42a{order:4;}}
@media only screen and (max-width: 1000px){._a039f42a{order:5;}}
@media only screen and (max-width: 900px){._a039f42a{order:6;}}
@media only screen and (max-width: 750px){._a039f42a{order:7;}}
@media only screen and (max-width: 500px){._a039f42a{order:8;}}
                    """)
                        ]
        , test "it works with user preference queries" <|
            \() ->
                div
                    [ css
                        [ MediaQuery.fromList
                            [ highContrastMode [ borderWidth (px 1) ]
                            , MediaQuery.not highContrastMode [ borderWidth (px 2) ]
                            ]
                        ]
                    ]
                    []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text "border-width:1px;"
                        , Selector.text "border-width:2px;"
                        ]
        , test "it works with duplicated queries" <|
            \() ->
                div
                    [ css
                        [ MediaQuery.fromList
                            [ prefersReducedMotion [ borderWidth (px 1) ]
                            , prefersReducedMotion [ fontSize (px 1) ]
                            ]
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
