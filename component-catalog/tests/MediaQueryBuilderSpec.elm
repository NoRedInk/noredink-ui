module MediaQueryBuilderSpec exposing (suite)

import Css exposing (int, order)
import Html.Styled exposing (div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.MediaQuery.V2 as MediaQuery exposing (mobile, narrowMobile, quizEngineMobile)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "MediaQuery.builder"
        [ test "it puts queries in the correct order" <|
            \() ->
                div
                    [ css <|
                        MediaQuery.toStyles
                            [ narrowMobile [ order (int 6) ]
                            , quizEngineMobile [ order (int 5) ]
                            , mobile [ order (int 4) ]
                            , MediaQuery.not narrowMobile [ order (int 1) ]
                            , MediaQuery.not quizEngineMobile [ order (int 2) ]
                            , MediaQuery.not mobile [ order (int 3) ]
                            ]
                    ]
                    []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text (String.trim """
@media only screen and (min-width: 501px){._543c77e6{order:1;}}
@media only screen and (min-width: 751px){._543c77e6{order:2;}}
@media only screen and (min-width: 1001px){._543c77e6{order:3;}}
@media only screen and (max-width: 1000px){._543c77e6{order:4;}}
@media only screen and (max-width: 750px){._543c77e6{order:5;}}
@media only screen and (max-width: 500px){._543c77e6{order:6;}}
                    """)
                        ]
        ]
