module MediaQueryBuilderSpec exposing (suite)

import Css exposing (fontSize, px)
import Html.Styled exposing (div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "MediaQuery.builder"
        [ test "it puts queries in the correct order" <|
            \() ->
                div
                    [ css
                        (MediaQuery.builder [ fontSize (px 99) ]
                            |> MediaQuery.onNarrowMobile [ fontSize (px 1) ]
                            |> MediaQuery.onQuizEngineMobile [ fontSize (px 2) ]
                            |> MediaQuery.onMobile [ fontSize (px 3) ]
                            |> MediaQuery.onDesktop [ fontSize (px 4) ]
                            |> MediaQuery.toStyles
                        )
                    ]
                    []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text (String.trim """
._ebc36d6{font-size:99px;}
@media only screen and (max-width: 1000px){._ebc36d6{font-size:3px;}}
@media only screen and (max-width: 750px){._ebc36d6{font-size:2px;}}
@media only screen and (max-width: 500px){._ebc36d6{font-size:1px;}}
@media only screen and (min-width: 1001px){._ebc36d6{font-size:4px;}}
                    """)
                        ]
        , test "it works with only a single breakpoint" <|
            \() ->
                div
                    [ css
                        (MediaQuery.builder [ fontSize (px 99) ]
                            |> MediaQuery.onMobile [ fontSize (px 3) ]
                            |> MediaQuery.toStyles
                        )
                    ]
                    []
                    |> toUnstyled
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "style" ]
                    |> Query.has
                        [ Selector.text (String.trim """
._d0428ccb{font-size:99px;}
@media only screen and (max-width: 1000px){._d0428ccb{font-size:3px;}}
                    """)
                        ]
        ]
