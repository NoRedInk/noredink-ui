module Spec.Nri.Ui.Mark exposing (..)

import Accessibility.Styled.Style exposing (invisibleStyle)
import Css
import Expect
import Markdown.Block
import Markdown.Inline
import Nri.Ui.Mark.V6 as Mark
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


{-| Cloned from Mark for testing
-}
highlightDescription : String -> List Mark.Mark -> String
highlightDescription prefix marks =
    let
        names =
            String.Extra.toSentenceOxford (List.filterMap (.name >> Maybe.map stripMarkdownSyntax) marks)
    in
    if names == "" then
        prefix ++ " highlight"

    else if List.length marks == 1 then
        prefix ++ " " ++ names ++ " highlight"

    else
        prefix ++ " " ++ names ++ " highlights"


{-| Cloned from Mark for testing
-}
stripMarkdownSyntax : String -> String
stripMarkdownSyntax markdown =
    case Markdown.Block.parse Nothing markdown of
        [ Markdown.Block.Paragraph _ inlines ] ->
            Markdown.Inline.extractText inlines

        _ ->
            markdown


{-| Cloned from Mark for testing
-}
cssContent : String -> Css.Style
cssContent content =
    Css.property "content" ("\" " ++ content ++ " \"")


{-| Cloned from Mark for testing
-}
tagBeforeContent : List Mark.Mark -> List Css.Style
tagBeforeContent marks =
    if List.isEmpty marks then
        []

    else
        [ Css.before
            [ cssContent (highlightDescription "start" marks)
            , invisibleStyle
            ]
        ]


{-| Cloned from Mark for testing
-}
tagAfterContent : List Mark.Mark -> List Css.Style
tagAfterContent marks =
    if List.isEmpty marks then
        []

    else
        [ Css.after
            [ cssContent (highlightDescription "end" marks)
            , invisibleStyle
            ]
        ]


type MarkStartTag
    = MarkStartTag


hasStartTag : Maybe a -> Maybe MarkStartTag
hasStartTag =
    Maybe.map (\_ -> MarkStartTag)


{-| Mark.overlappingStyles gives us a (Maybe (Html msg)) for the label outputs,
but we can't test equality on HTML elements very well so we need to drop it.
-}
testOverlappingStyles : List ( content, List Mark.Mark ) -> List ( content, Maybe MarkStartTag, List Css.Style )
testOverlappingStyles inputs =
    Mark.overlappingStyles inputs
        |> List.map (\( content, maybeStartTag, styles_ ) -> ( content, hasStartTag maybeStartTag, styles_ ))


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
                            [ ( 1, Nothing, [] )
                            , ( 2, Nothing, [] )
                            , ( 3, Nothing, [] )
                            ]
            , test "mark on single segment" <|
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
                    , ( 3, [] )
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, Nothing, [] )
                            , ( 2
                              , Nothing
                              , tagBeforeContent [ mark ]
                                    ++ startStyles
                                    ++ markStyles
                                    ++ tagAfterContent [ mark ]
                                    ++ endStyles
                              )
                            , ( 3, Nothing, [] )
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
                            [ ( 1, Nothing, [] )
                            , ( 2
                              , Nothing
                              , tagBeforeContent [ mark ]
                                    ++ startStyles
                                    ++ markStyles
                              )
                            , ( 3
                              , Nothing
                              , markStyles
                                    ++ tagAfterContent [ mark ]
                                    ++ endStyles
                              )
                            , ( 4, Nothing, [] )
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
                            [ ( 1, Nothing, [] )
                            , ( 2, Just MarkStartTag, markStyles ++ tagAfterContent [ mark ] ++ endStyles )
                            , ( 3, Nothing, [] )
                            ]
            , test "named mark end segment" <|
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
                    ]
                        |> testOverlappingStyles
                        |> Expect.equalLists
                            [ ( 1, Nothing, [] )
                            , ( 2, Just MarkStartTag, markStyles ++ tagAfterContent [ mark ] ++ endStyles )
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
                            [ ( 1, Nothing, [] )
                            , ( 2, Just MarkStartTag, markStyles )
                            , ( 3, Nothing, markStyles ++ tagAfterContent [ mark ] ++ endStyles )
                            , ( 4, Nothing, [] )
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
                            [ ( 1, Nothing, [] )
                            , ( 2, Just MarkStartTag, markStyles ++ tagAfterContent [ mark1 ] ++ endStyles )
                            , ( 3, Just MarkStartTag, mark2Styles ++ tagAfterContent [ mark2 ] ++ end2Styles )
                            , ( 4, Nothing, [] )
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
                            [ ( 1, Nothing, [] )
                            , ( 2
                              , Just MarkStartTag
                              , markStyles
                                    ++ mark2Styles
                                    ++ tagAfterContent [ mark1, mark2 ]
                                    ++ endStyles
                                    ++ end2Styles
                              )
                            , ( 3, Nothing, [] )
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
                            [ ( 1, Nothing, [] )
                            , ( 2, Just MarkStartTag, markStyles ++ mark2Styles ++ tagAfterContent [ mark1 ] ++ endStyles )
                            , ( 3, Nothing, mark2Styles ++ tagAfterContent [ mark2 ] ++ end2Styles )
                            , ( 4, Nothing, [] )
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
                            [ ( 1, Nothing, [] )
                            , ( 2, Just MarkStartTag, markStyles )
                            , ( 3, Just MarkStartTag, markStyles ++ mark2Styles ++ tagAfterContent [ mark2 ] ++ end2Styles )
                            , ( 4, Nothing, markStyles ++ tagAfterContent [ mark1 ] ++ endStyles )
                            , ( 5, Nothing, [] )
                            ]
            ]
        ]
