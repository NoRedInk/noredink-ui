module Spec.Nri.Ui.Review.MemoizedFocusLoopLazy exposing (..)

import Nri.Ui.ElmReview.MemoizedFocusLoopLazy exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


withHeader : String -> String
withHeader body =
    """
module A exposing (..)
import Nri.Ui.FocusLoop.Lazy.V1 as Lazy
import Html exposing (text)

""" ++ body


all : Test
all =
    describe "MemoizedFocusLoopLazy"
        [ test "Passes if lazy is memoized" <|
            \_ ->
                withHeader """
f = Lazy.lazy {}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "Fails if lazy application is not the top expression" <|
            \_ ->
                withHeader """
f = let g = Lazy.lazy view
     in g
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calls to lazy should be memoized at the top level of a view function."
                            , details = [ "See here" ]
                            , under = "Lazy.lazy"
                            }
                        ]
        , test "Fails if lazy application is not point-free" <|
            \_ ->
                withHeader """
f x = Lazy.lazy view x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Calls to lazy should be memoized at the top level of a view function."
                            , details = [ "See here" ]
                            , under = "Lazy.lazy"
                            }
                        ]
        ]
