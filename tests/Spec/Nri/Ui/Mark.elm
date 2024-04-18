module Spec.Nri.Ui.Mark exposing (..)

import Accessibility.Styled.Aria as Aria
import Content
import Css
import Expect
import Html.Styled as Html exposing (Html, span)
import Html.Styled.Attributes exposing (class, css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Mark.V6 as Mark
import Nri.Ui.MediaQuery.V1 as MediaQuery
import String.Extra
import Test exposing (..)


startStyles : List Css.Style
startStyles =
    [ Css.color (Css.hex "000000") ]


markStyles : List Css.Style
markStyles =
    [ Css.fontWeight Css.bold ]


endStyles : List Css.Style
endStyles =
    [ Css.position Css.absolute ]


start2Styles : List Css.Style
start2Styles =
    [ Css.color (Css.hex "222222") ]


mark2Styles : List Css.Style
mark2Styles =
    [ Css.fontWeight Css.bolder ]


end2Styles : List Css.Style
end2Styles =
    [ Css.position Css.fixed ]


{-| Cloned from Mark.V6 for testing
-}
viewLabels : List String -> List Css.Style -> Html msg
viewLabels names startStyles_ =
    span [ css startStyles_ ]
        [ span
            [ css
                [ Fonts.baseFont
                , Css.backgroundColor Colors.white
                , Css.color Colors.navy
                , Css.padding2 (Css.px 2) (Css.px 4)
                , Css.borderRadius (Css.px 3)
                , Css.margin2 Css.zero (Css.px 5)
                , Css.boxShadow5 Css.zero (Css.px 1) (Css.px 1) Css.zero Colors.gray75
                , Css.batch
                    [ Css.display Css.none
                    , MediaQuery.highContrastMode
                        [ Css.property "forced-color-adjust" "none"
                        , Css.display Css.inline |> Css.important
                        , Css.property "color" "initial" |> Css.important
                        ]
                    ]
                ]
            , -- we use the :before element to convey details about the start of the
              -- highlighter to screenreaders, so the visual label is redundant
              Aria.hidden True
            ]
            (Content.markdownInline (String.Extra.toSentenceOxford names))
        ]


{-| Mark.overlappingStyles gives us a (Maybe (Html msg)) for the label outputs,
but we can't test equality on HTML elements very well so we need to drop it.
-}
testOverlappingStyles : List ( content, List Mark.Mark ) -> List ( content, List Css.Style )
testOverlappingStyles inputs =
    Mark.overlappingStyles inputs
        |> List.map (\( content, _, styles_ ) -> ( content, styles_ ))


tests : Test
tests =
    describe "Nri.Ui.Mark"
        [ describe "overlappingStyles"
            [ test "with no marks" <|
                \_ ->
                    [ ( 1, [] )
                    , ( 2, [] )
                    , ( 3, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, [] )
                            , ( 3, [] )
                            ]
            , test "mark on single segment" <|
                \_ ->
                    [ ( 1, [] )
                    , ( 2
                      , [ { name = Nothing
                          , startStyles = startStyles
                          , styles = markStyles
                          , endStyles = endStyles
                          }
                        ]
                      )
                    , ( 3, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, startStyles ++ markStyles ++ endStyles )
                            , ( 3, [] )
                            ]
            , test "mark spanning multiple segments" <|
                \_ ->
                    let
                        mark =
                            { name = Nothing
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark ] )
                    , ( 3, [ mark ] )
                    , ( 4, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, startStyles ++ markStyles )
                            , ( 3, markStyles ++ endStyles )
                            , ( 4, [] )
                            ]
            , test "named mark one segment" <|
                \_ ->
                    let
                        mark =
                            { name = Just "mark"
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark ] )
                    , ( 3, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, markStyles ++ endStyles )
                            , ( 3, [] )
                            ]
            , test "named mark two segments" <|
                \_ ->
                    let
                        mark =
                            { name = Just "mark"
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark ] )
                    , ( 3, [ mark ] )
                    , ( 4, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, markStyles )
                            , ( 3, markStyles ++ endStyles )
                            , ( 4, [] )
                            ]
            , test "two marks" <|
                \_ ->
                    let
                        mark1 =
                            { name = Just "mark1"
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }

                        mark2 =
                            { name = Just "mark2"
                            , startStyles = start2Styles
                            , styles = mark2Styles
                            , endStyles = end2Styles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark1 ] )
                    , ( 3, [ mark2 ] )
                    , ( 4, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, markStyles ++ endStyles )
                            , ( 3, mark2Styles ++ end2Styles )
                            , ( 4, [] )
                            ]
            , test "two marks full overlap" <|
                \_ ->
                    let
                        mark1 =
                            { name = Just "mark1"
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }

                        mark2 =
                            { name = Just "mark2"
                            , startStyles = start2Styles
                            , styles = mark2Styles
                            , endStyles = end2Styles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark1, mark2 ] )
                    , ( 3, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, markStyles ++ mark2Styles ++ end2Styles ++ endStyles )
                            , ( 3, [] )
                            ]
            , test "two marks partial overlap" <|
                \_ ->
                    let
                        mark1 =
                            { name = Just "mark1"
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }

                        mark2 =
                            { name = Just "mark2"
                            , startStyles = start2Styles
                            , styles = mark2Styles
                            , endStyles = end2Styles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark1, mark2 ] )
                    , ( 3, [ mark2 ] )
                    , ( 4, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, markStyles ++ mark2Styles ++ endStyles )
                            , ( 3, mark2Styles ++ end2Styles )
                            , ( 4, [] )
                            ]
            , test "two marks containment" <|
                \_ ->
                    let
                        mark1 =
                            { name = Just "mark1"
                            , startStyles = startStyles
                            , styles = markStyles
                            , endStyles = endStyles
                            }

                        mark2 =
                            { name = Just "mark2"
                            , startStyles = start2Styles
                            , styles = mark2Styles
                            , endStyles = end2Styles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark1 ] )
                    , ( 3, [ mark1, mark2 ] )
                    , ( 4, [ mark1 ] )
                    , ( 5, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, [] )
                            , ( 2, markStyles )
                            , ( 3, markStyles ++ mark2Styles ++ end2Styles )
                            , ( 4, markStyles ++ endStyles )
                            , ( 5, [] )
                            ]
            ]
        ]
