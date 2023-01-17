module Spec.InputErrorAndGuidance exposing (spec)

import Accessibility.Aria as Aria
import Css
import Expect exposing (Expectation)
import Html.Styled
import Html.Styled.Attributes
import InputErrorAndGuidanceInternal exposing (ErrorState, Guidance)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


spec : Test
spec =
    describe "InputErrorAndGuidanceInternal"
        [ test "Renders an empty node when no guidance or error is present" <|
            \() ->
                viewQuery emptyErrorAndGuidance
                    |> Expect.all
                        [ Query.hasNot [ Selector.id guidanceId ]
                        , Query.hasNot [ Selector.id errorId ]
                        ]
        , test "Renders a guidance node when guidance is present and error is not" <|
            \() ->
                emptyErrorAndGuidance
                    |> InputErrorAndGuidanceInternal.setGuidance "Password must be at least 8 characters long."
                    |> viewQuery
                    |> Expect.all
                        [ hasInputDescribedBy [ guidanceId ]
                        , hasGuidance "Password must be at least 8 characters long."
                        , Query.hasNot [ Selector.id errorId ]
                        ]
        , test "Renders an error node when error is present and guidance is not" <|
            \() ->
                emptyErrorAndGuidance
                    |> InputErrorAndGuidanceInternal.setErrorMessage (Just "Password must be at least 8 characters long.")
                    |> viewQuery
                    |> Expect.all
                        [ hasInputDescribedBy [ errorId ]
                        , hasError "Password must be at least 8 characters long."
                        , Query.hasNot [ Selector.id guidanceId ]
                        ]
        ]


emptyErrorAndGuidance : { guidance : Guidance, error : ErrorState }
emptyErrorAndGuidance =
    { guidance = InputErrorAndGuidanceInternal.noGuidance
    , error = InputErrorAndGuidanceInternal.noError
    }


viewQuery : { guidance : Guidance, error : ErrorState } -> Query.Single msg
viewQuery config =
    Html.Styled.div []
        [ Html.Styled.input
            [ Html.Styled.Attributes.id inputId
            , InputErrorAndGuidanceInternal.describedBy inputId config
            ]
            []
        , InputErrorAndGuidanceInternal.view inputId (Css.batch []) config
        ]
        |> Html.Styled.toUnstyled
        |> Query.fromHtml


inputId : String
inputId =
    "input-id"


guidanceId : String
guidanceId =
    InputErrorAndGuidanceInternal.guidanceId inputId


errorId : String
errorId =
    InputErrorAndGuidanceInternal.errorId inputId


hasInputDescribedBy : List String -> Query.Single msg -> Expectation
hasInputDescribedBy ids =
    Query.has
        [ Selector.all
            [ Selector.id inputId
            , Selector.attribute (Aria.describedBy ids)
            ]
        ]


hasGuidance : String -> Query.Single msg -> Expectation
hasGuidance content =
    Query.has
        [ Selector.id guidanceId
        , Selector.containing [ Selector.text content ]
        ]


hasError : String -> Query.Single msg -> Expectation
hasError content =
    Query.has
        [ Selector.id errorId
        , Selector.containing [ Selector.text content ]
        ]
