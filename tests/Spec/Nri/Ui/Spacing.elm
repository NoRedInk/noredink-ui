module Spec.Nri.Ui.Spacing exposing (all)

import Css
import Expect exposing (Expectation)
import Html.Styled as Html exposing (div)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Spacing.V1 as Spacing
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "Nri.Ui.Spacing.V1"
        [ describe "centeredContentWithSidePadding"
            [ test "keeps its gutters on through the zone just above the breakpoint" <|
                \() ->
                    -- The auto margins that center the content shrink to zero
                    -- as the viewport approaches the breakpoint. If the padding
                    -- activates any lower than breakpoint + 2 gutters, there is
                    -- a band of viewport widths where the content sits flush
                    -- against the viewport edges.
                    emittedCss Spacing.centeredContentWithSidePadding
                        |> Expect.all
                            [ expectContains "max-width:1000px"
                            , expectContains "@media only screen and (max-width: 1030px)"
                            ]
            , test "goes full-bleed inside the gutter zone so the gutter width is constant" <|
                \() ->
                    -- Keeping the max width inside the zone would stack the
                    -- centering margins on top of the padding, making the
                    -- gutter jump at the zone's outer edge.
                    emittedCss Spacing.centeredContentWithSidePadding
                        |> expectContains "max-width:none;padding-left:15px;padding-right:15px"
            ]
        , describe "centeredContentWithSidePaddingAndCustomWidth"
            [ test "keeps the historical narrower activation for custom page widths" <|
                \() ->
                    -- Pages that pass a custom width pair this style with
                    -- headers and sidebars that are not breakpoint-aware, so
                    -- their gutters have to change together, page by page.
                    emittedCss (Spacing.centeredContentWithSidePaddingAndCustomWidth (Css.px 1380))
                        |> Expect.all
                            [ expectContains "max-width:1380px"
                            , expectContains "(max-width: 1350px)"
                            , expectLacks "max-width:none"
                            ]
            ]
        , describe "centeredContent"
            [ test "reserves no gutters, so it can sit flush against the viewport edges" <|
                \() ->
                    emittedCss Spacing.centeredContent
                        |> Expect.all
                            [ expectContains "max-width:1000px"
                            , expectLacks "padding"
                            ]
            ]
        ]


emittedCss : Css.Style -> String
emittedCss style =
    div [ css [ style ] ] []
        |> Html.toUnstyled
        |> Query.fromHtml
        |> Query.find [ Selector.tag "style" ]
        |> Query.children []
        |> Debug.toString


expectContains : String -> String -> Expectation
expectContains needle haystack =
    if String.contains needle haystack then
        Expect.pass

    else
        Expect.fail (report "to contain" needle haystack)


expectLacks : String -> String -> Expectation
expectLacks needle haystack =
    if String.contains needle haystack then
        Expect.fail (report "not to contain" needle haystack)

    else
        Expect.pass


report : String -> String -> String -> String
report expectation needle haystack =
    "Expected the emitted CSS "
        ++ expectation
        ++ " `"
        ++ needle
        ++ "`, but it was:\n\n"
        ++ haystack
