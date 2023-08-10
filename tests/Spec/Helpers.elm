module Spec.Helpers exposing (..)

import Html.Attributes as Attributes
import Test.Html.Selector as Selector


nriDescription : String -> Selector.Selector
nriDescription desc =
    Selector.attribute (Attributes.attribute "data-nri-description" desc)
