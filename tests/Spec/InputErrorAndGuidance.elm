module Spec.InputErrorAndGuidance exposing (spec)

import Css
import Expect
import Html.Styled
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
                    |> Query.children []
                    |> Query.count (Expect.equal 0)
        ]


emptyErrorAndGuidance : { guidance : Guidance, error : ErrorState }
emptyErrorAndGuidance =
    { guidance = InputErrorAndGuidanceInternal.noGuidance
    , error = InputErrorAndGuidanceInternal.noError
    }


viewQuery : { guidance : Guidance, error : ErrorState } -> Query.Single msg
viewQuery =
    InputErrorAndGuidanceInternal.view "test-id" (Css.batch [])
        >> Html.Styled.toUnstyled
        >> Query.fromHtml
