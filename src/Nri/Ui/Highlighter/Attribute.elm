module Nri.Ui.Highlighter.Attribute exposing (..)

{-| An Attribute class for Highlighter/Highlightable/Mark that is comparable

This should help us make highlighter views lazy

-}

import Html
import Html.Attributes


type Attribute
    = Class String


toHtmlAttribute : Attribute -> Html.Attribute msg
toHtmlAttribute attribute =
    case attribute of
        Class value ->
            Html.Attributes.class value
