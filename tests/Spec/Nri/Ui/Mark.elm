module Spec.Nri.Ui.Mark exposing (..)

import Css
import Expect
import Nri.Ui.Mark.V6 as Mark
import Test exposing (..)


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
                        |> Mark.overlappingStyles
                        |> Expect.equalLists
                            [ ( 1, Nothing, [] )
                            , ( 2, Nothing, [] )
                            , ( 3, Nothing, [] )
                            ]
            , test "mark on single segment" <|
                \_ ->
                    let
                        startStyles =
                            [ Css.color (Css.hex "000000") ]

                        styles =
                            [ Css.fontWeight Css.bold ]

                        endStyles =
                            [ Css.position Css.absolute ]
                    in
                    [ ( 1, [] )
                    , ( 2
                      , [ { name = Nothing
                          , startStyles = startStyles
                          , styles = styles
                          , endStyles = endStyles
                          }
                        ]
                      )
                    , ( 3, [] )
                    ]
                        |> Mark.overlappingStyles
                        |> Expect.equalLists
                            [ ( 1, Nothing, [] )
                            , ( 2, Nothing, startStyles ++ styles ++ endStyles )
                            , ( 3, Nothing, [] )
                            ]
            , test "mark spanning multiple segments" <|
                \_ ->
                    let
                        startStyles =
                            [ Css.color (Css.hex "000000") ]

                        styles =
                            [ Css.fontWeight Css.bold ]

                        endStyles =
                            [ Css.position Css.absolute ]

                        mark =
                            { name = Nothing
                            , startStyles = startStyles
                            , styles = styles
                            , endStyles = endStyles
                            }
                    in
                    [ ( 1, [] )
                    , ( 2, [ mark ] )
                    , ( 3, [ mark ] )
                    , ( 4, [] )
                    ]
                        |> Mark.overlappingStyles
                        |> Expect.equalLists
                            [ ( 1, Nothing, [] )
                            , ( 2, Nothing, startStyles ++ styles )
                            , ( 3, Nothing, styles ++ endStyles )
                            , ( 4, Nothing, [] )
                            ]
            ]
        ]
