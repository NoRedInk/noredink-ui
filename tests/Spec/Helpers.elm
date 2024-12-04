module Spec.Helpers exposing (..)

import Expect exposing (Expectation)
import Html.Attributes as Attributes
import Test.Html.Selector as Selector
import Test.Runner


nriDescription : String -> Selector.Selector
nriDescription desc =
    Selector.attribute (Attributes.attribute "data-nri-description" desc)


expectFailure : String -> Expectation -> Expectation
expectFailure expectedDescriptionSubstring expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.fail "Expected a failure, but there was none."

        Just reason ->
            String.contains expectedDescriptionSubstring reason.description
                |> Expect.equal True
