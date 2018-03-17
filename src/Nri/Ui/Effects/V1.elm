module Nri.Ui.Effects.V1 exposing (selectionShadow)

{-| Css mixins reused across Nri modules.

@docs selectionShadow

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import Nri.Ui.Colors.V1


{-| Draw a 2 px thick ochre border around the element to indicate it is
selected.

This uses a CSS box shadow to draw what looks like a border. Box shadows are
perfect for this because they don't affect the elements positioning in any way.
This means we can be sure switching the selection shadow on and off is not
going to make the element jump.

-}
selectionShadow : List Style
selectionShadow =
    -- There should appear to be 2px of space between the element outline and
    -- the surrounding selection border. To accomplish this we use two box
    -- shadows, a inner white shadow and an outer ochre one.
    -- Elm-css does not support multiple box shadows, so we build up that the
    -- CSS value manually.
    [ Css.property "box-shadow" ("0 0 0 2px white, 0 0 0 4px " ++ colorToString Nri.Ui.Colors.V1.ochre)
    ]


colorToString : Css.Color -> String
colorToString { red, green, blue } =
    String.concat [ "rgb(", toString red, ",", toString green, ",", toString blue, ")" ]
