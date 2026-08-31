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
        [ describe "centeredContentWithSidePaddingAndCustomWidth"
            [ test "reserves the gutters at every width, outside the content's max width" <|
                \() ->
                    -- Gating the gutters behind a media query instead leaves the
                    -- content flush against the viewport edges between the width
                    -- where the auto margins run out and the width where the
                    -- query fires.
                    emittedCss (Spacing.centeredContentWithSidePaddingAndCustomWidth (Css.px 800))
                        |> Expect.all
                            [ expectContains "max-width:830px"
                            , expectContains "padding-left:15px;padding-right:15px"
                            , expectContains "box-sizing:border-box"
                            , expectLacks "@media"
                            ]
            , test "still lays the content out to the breakpoint on a roomy viewport" <|
                \() ->
                    -- 830px of border box minus 15px of gutter on each side.
                    emittedCss Spacing.centeredContentWithSidePadding
                        |> expectContains "max-width:1030px"
            ]
        , describe "centeredContentWithCustomWidth"
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
