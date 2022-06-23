module Nri.Ui.FocusRing.V1 exposing
    ( forKeyboardUsers, forMouseUsers
    , styles
    , boxShadows, outerBoxShadow, insetBoxShadow
    , customClass
    )

{-|

@docs forKeyboardUsers, forMouseUsers
@docs styles
@docs boxShadows, outerBoxShadow, insetBoxShadow
@docs customClass

-}

import Css
import Css.Global exposing (Snippet)
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors


{-| When :focus-visible, add the two-tone focus ring.

Hides default focus ring from elements that are tagged as having a custom focus ring.

-}
forKeyboardUsers : List Css.Global.Snippet
forKeyboardUsers =
    [ Css.Global.class customClass [ Css.outline Css.none ]
    , Css.Global.selector (":not(." ++ customClass ++ "):focus-visible") styles
    ]


{-| -}
forMouseUsers : List Snippet
forMouseUsers =
    [ Css.Global.everything [ Css.outline Css.none ]
    , Css.Global.selector ":focus-within .checkbox-icon-container"
        [ Css.important (Css.boxShadow Css.none)
        ]
    , Css.Global.selector ":focus-within .Nri-RadioButton-RadioButtonIcon"
        [ Css.important (Css.boxShadow Css.none)
        ]
    ]


{-| Add this class to remove global focus styles. Only do this
if you'll be adding the two-tone focus ring styles another way.
-}
customClass : String
customClass =
    "custom-focus-ring"


{-| A two-tone focus ring that will be visually apparent for any background/element combination.

NOTE: use `boxShadows` instead if your focusable element:

  - already has a box shadow
  - has an explicit border radius set

-}
styles : List Css.Style
styles =
    [ boxShadows []
    , Css.outline Css.none
    , Css.borderRadius (Css.px 4)
    ]


{-|

    focus
        [ FocusRing.boxShadows [ "inset 0 3px 0 0 " ++ ColorsExtra.toCssString glacier ]
        , outline none
        ]

-}
boxShadows : List String -> Css.Style
boxShadows existingBoxShadows =
    existingBoxShadows
        ++ [ "0 0 0 3px " ++ toCssString Colors.white
           , "0 0 0 6px " ++ toCssString Colors.red
           ]
        |> String.join ","
        |> -- using `property` due to https://github.com/rtfeldman/elm-css/issues/265
           Css.property "box-shadow"


{-| In special cases, we don't use a two-tone focus ring, and an outset focus ring would be obscured.

Be very sure this is what you need before using this!

-}
insetBoxShadow : Css.Style
insetBoxShadow =
    -- using `property` due to https://github.com/rtfeldman/elm-css/issues/265
    Css.property "box-shadow" ("inset 0 0 0 3px " ++ toCssString Colors.red)


{-| In special cases, we don't use a two-tone focus ring.

Be very sure this is what you need before using this!

-}
outerBoxShadow : Css.Style
outerBoxShadow =
    -- using `property` due to https://github.com/rtfeldman/elm-css/issues/265
    Css.property "box-shadow" ("0 0 0 3px " ++ toCssString Colors.red)
