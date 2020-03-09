module Nri.Ui.CssVendorPrefix.V1 exposing (property, value, complexProperty)

{-| Vendor prefixed css properties.

@docs property, value, complexProperty

-}

import Css


{-| Css vendor prefixes
-}
prefixes : List String
prefixes =
    [ "-webkit-", "-moz-", "-o-", "-ms-", "" ]


{-| Same as Css.property but vendor prefixed.
-}
property : String -> String -> Css.Style
property prop value_ =
    prefixes
        |> List.map
            (\prefix ->
                Css.property (prefix ++ prop) value_
            )
        |> Css.batch


{-| Same as Css.property but vendor prefixed.
-}
value : String -> String -> Css.Style
value prop value_ =
    prefixes
        |> List.map
            (\prefix ->
                Css.property prop (prefix ++ value_)
            )
        |> Css.batch


{-| Used to build more complex Css styles
-}
complexProperty : (String -> Css.Style) -> Css.Style
complexProperty buildProp =
    prefixes
        |> List.map buildProp
        |> Css.batch
