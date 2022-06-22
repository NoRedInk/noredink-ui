module Nri.Ui.FocusRing.V1 exposing
    ( styles, focusVisibleStyles
    , boxShadows, outerBoxShadow
    , customClass
    )

{-|

@docs styles, focusVisibleStyles
@docs boxShadows, outerBoxShadow
@docs customClass

-}

import Css
import Css.Global
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors


{-| Add this class to remove global focus styles. Only do this
if you'll be adding the two-tone focus ring styles another way.
-}
customClass : String
customClass =
    "custom-focus-ring"


{-| When :focus-visible, add the two-tone focus ring.
-}
focusVisibleStyles : Css.Global.Snippet
focusVisibleStyles =
    Css.Global.selector (":not(." ++ customClass ++ "):focus-visible") styles


{-| A two-tone focus ring that will be visually apparent for any background/element combination.
-}
styles : List Css.Style
styles =
    [ boxShadows []
    , Css.outline Css.none

    -- TODO: it isn't safe to set the border radius on focus.
    -- learn/mastery has anchor tag cards with border radius 8px, for instance
    , Css.borderRadius (Css.px 4)
    ]


{-| -}
boxShadows : List String -> Css.Style
boxShadows existingBoxShadows =
    existingBoxShadows
        ++ [ "0 0 0 3px " ++ toCssString Colors.white
           , "0 0 0 6px " ++ toCssString Colors.red
           ]
        |> String.join ","
        |> -- using `property` due to https://github.com/rtfeldman/elm-css/issues/265
           Css.property "box-shadow"


{-| In special cases, we don't use a two-tone focus ring. Be very sure this is what you need before using this!
-}
outerBoxShadow : Css.Style
outerBoxShadow =
    -- using `property` due to https://github.com/rtfeldman/elm-css/issues/265
    Css.property "box-shadow" ("0 0 0 3px " ++ toCssString Colors.red)
